*&---------------------------------------------------------------------*
*& Report  DEMOEXCELINTEGRATION                                        *
*&---------------------------------------------------------------------*
INCLUDE RDEMOEXCELINTEGRATION2TOP.

DATA: CONTROL TYPE REF TO I_OI_CONTAINER_CONTROL.
DATA: CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
DATA: LINK_SERVER TYPE REF TO I_OI_LINK_SERVER.
DATA: TABLE_COLL TYPE REF TO I_OI_TABLE_COLLECTION.

DATA: DOCUMENT_TYPE TYPE SOI_DOCUMENT_TYPE
                                       VALUE SOI_DOCTYPE_EXCEL_CHART,
      DOCUMENT_FORMAT TYPE SOI_DOCUMENT_TYPE.

DATA: DOC_SIZE TYPE I, DOC_URL TYPE T_URL,
      DOC_TABLE TYPE SBDST_CONTENT.

TYPES: T_OI_RET_STRING TYPE SOI_RET_STRING.
DATA: RETCODE TYPE T_OI_RET_STRING.

*---Global Variables & Tables
FIELD-SYMBOLS: <MONTH>,
               <YEAR> .

TABLES: ZTPM_SHOP,
        ZTPM_ANBD,
        ZTPM_OPTIME,
        ZTPM_MONBD.

*** Base Shop list
DATA: BEGIN OF IT_SHOP OCCURS 0,
        SHOP LIKE ZTPM_SHOP-SHOP,
      END OF IT_SHOP.

*** Base Year list
DATA: BEGIN OF IT_YEAR OCCURS 0,
         AJAHR LIKE ZTPM_OPTIME-AJAHR,
      END OF IT_YEAR.

DATA: BEGIN OF IT_RANGE OCCURS 0,
*         AJAHR  LIKE ZTPM_MONBD-AJAHR,
         ZMONTH LIKE ZTPM_MONBD-ZMONTH,
      END OF IT_RANGE.

*** Annually average breakdown rate
DATA: IT_ZTPM_ANBD LIKE ZTPM_ANBD OCCURS 0 WITH HEADER LINE.

*** Monthly average breakdown rate
DATA: IT_RATE LIKE ZSPM_BDMON OCCURS 0 WITH HEADER LINE.

DATA: IT_TEMP_MONBD LIKE ZSPM_MONTH2 OCCURS 0.
DATA: IT_MONBD LIKE ZSPM_MONTH2 OCCURS 0 WITH HEADER LINE.

DATA: IT_TEMP_ANBD LIKE ZSPM_YEAR OCCURS 0.
DATA: IT_ANBD LIKE ZSPM_YEAR OCCURS 0 WITH HEADER LINE.

*** Global variables
DATA: WA_S_YEAR LIKE ZTPM_ANBD-AJAHR,
      WA_E_YEAR LIKE ZTPM_ANBD-AJAHR,
      WA_SHTXT LIKE ZTPM_SHOP-SHTXT,
      WA_INIT.

*---------------------------------------------------------------------*
*       CLASS c_excel_document DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS C_EXCEL_DOCUMENT DEFINITION.

  PUBLIC SECTION.
    DATA: PROXY TYPE REF TO I_OI_DOCUMENT_PROXY.
    DATA: DOCUMENT_TYPE TYPE SOI_DOCUMENT_TYPE.
    DATA: DATA_TABLE TYPE SBDST_CONTENT,
          DATA_SIZE TYPE I,
          DOC_URL TYPE T_URL.

    METHODS: CONSTRUCTOR
              IMPORTING CONTROL TYPE REF TO I_OI_CONTAINER_CONTROL
                        DOCUMENT_TYPE TYPE SOI_DOCUMENT_TYPE.

    METHODS: ON_CLOSE_DOCUMENT
              FOR EVENT ON_CLOSE_DOCUMENT OF I_OI_DOCUMENT_PROXY
              IMPORTING DOCUMENT_PROXY HAS_CHANGED.

    METHODS: ON_CUSTOM_EVENT
              FOR EVENT ON_CUSTOM_EVENT OF I_OI_DOCUMENT_PROXY
              IMPORTING DOCUMENT_PROXY EVENT_NAME PARAM_COUNT
                        PARAM1 PARAM2 PARAM3.

    METHODS: CREATE_DOCUMENT
                  IMPORTING OPEN_INPLACE    TYPE C DEFAULT ' '
                            VALUE(NO_FLUSH) TYPE C DEFAULT ' '
                  EXPORTING ERROR TYPE REF TO I_OI_ERROR.

    METHODS: OPEN_DOCUMENT
                  IMPORTING OPEN_INPLACE    TYPE C DEFAULT ' '
                            OPEN_READONLY   TYPE C DEFAULT ' '
                            VALUE(NO_FLUSH) TYPE C DEFAULT ' '
                  EXPORTING ERROR TYPE REF TO I_OI_ERROR.

    METHODS: OPEN_DOCUMENT_URL
                  IMPORTING OPEN_INPLACE  TYPE C DEFAULT ' '
                            OPEN_READONLY TYPE C DEFAULT ' '
                            DOC_URL TYPE T_URL DEFAULT ' '
                  EXPORTING ERROR TYPE REF TO I_OI_ERROR.

    METHODS: RETRIEVE_DOCUMENT
                 IMPORTING DOCUMENTS TYPE DOCUMENT_LIST
                 EXPORTING DOCUMENT_FORMAT TYPE SOI_DOCUMENT_TYPE
                           DOC_URL TYPE T_URL.

    METHODS: CLOSE_DOCUMENT
                  IMPORTING DO_SAVE         TYPE C DEFAULT ' '
                            VALUE(NO_FLUSH) TYPE C DEFAULT ' '
                  EXPORTING ERROR       TYPE REF TO I_OI_ERROR.

  PRIVATE SECTION.
    DATA: CONTROL  TYPE REF TO I_OI_CONTAINER_CONTROL.
ENDCLASS.


DATA: DOCUMENT TYPE REF TO C_EXCEL_DOCUMENT.
DATA: ERROR TYPE REF TO I_OI_ERROR.
DATA: ERROR_TABLE TYPE TABLE OF REF TO I_OI_ERROR.
DATA: BDS_INSTANCE TYPE REF TO CL_BDS_DOCUMENT_SET.

*********** SELECTION-SCREEN ***********************************
****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.

PARAMETER : P_MONTH LIKE ZTPM_MONBD-ZMONTH DEFAULT '199901'.
"SY-DATUM(6).

SELECT-OPTIONS : S_SHOP  FOR ZTPM_SHOP-SHOP  NO INTERVALS
                                             NO-EXTENSION.

PARAMETER : P_PLACE LIKE ZSPM_PARAM-INPLACE DEFAULT 'X'
                                            AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK BLOCK1.

******************* INITIALIZATION ********************************
*******************************************************************
INITIALIZATION.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_MONTH .
*  PERFORM SELECT_MONTH.

***************** AT SELECTION-SCREEN ******************************
********************************************************************
AT SELECTION-SCREEN.

  CASE SY-UCOMM.
    WHEN 'ONLI'.
      CLEAR: SY-UCOMM.
*      PERFORM READ_DATA.
      CALL SCREEN '0100'.

    WHEN OTHERS.
      CLEAR: SY-UCOMM.

  ENDCASE.
****************START-OF-SELECTION ************************************
***********************************************************************
START-OF-SELECTION.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'MAIN0100'.
  SET TITLEBAR '001'.

  IF CONTROL IS INITIAL.
    CLEAR ERROR_TABLE.

    DATA: B_HAS_ACTIVEX.

    CALL FUNCTION 'GUI_HAS_ACTIVEX'
         IMPORTING
              RETURN = B_HAS_ACTIVEX.
    IF B_HAS_ACTIVEX IS INITIAL. MESSAGE E007. ENDIF.

    CALL METHOD C_OI_CONTAINER_CONTROL_CREATOR=>GET_CONTAINER_CONTROL
                      IMPORTING CONTROL = CONTROL
                                ERROR = ERROR.
    APPEND ERROR TO ERROR_TABLE.

    CREATE OBJECT CONTAINER
              EXPORTING CONTAINER_NAME = 'CONTAINER'.

    CALL METHOD CONTROL->INIT_CONTROL
                        EXPORTING R3_APPLICATION_NAME =
                                              'R/3 Basis'   "#EC NOTEXT
                                  INPLACE_ENABLED = 'X'
                                  INPLACE_SCROLL_DOCUMENTS = 'X'
                                  PARENT = CONTAINER
                                  REGISTER_ON_CLOSE_EVENT = 'X'
                                  REGISTER_ON_CUSTOM_EVENT = 'X'
                                  NO_FLUSH = 'X'
                        IMPORTING ERROR = ERROR.
    APPEND ERROR TO ERROR_TABLE.

    CALL METHOD CONTROL->GET_LINK_SERVER
                       EXPORTING NO_FLUSH = 'X'
                       IMPORTING LINK_SERVER = LINK_SERVER
                                 ERROR = ERROR.
    APPEND ERROR TO ERROR_TABLE.

    CALL METHOD LINK_SERVER->START_LINK_SERVER
                       EXPORTING NO_FLUSH = 'X'
*                                 LINK_SERVER_MODE =
*                                 LINK_SERVER->LINK_SERVER_CUSTOMNAME
*                                 SERVER_NAME_SUFFIX = 'BDCH'
                       IMPORTING ERROR = ERROR.
    APPEND ERROR TO ERROR_TABLE.


    LOOP AT ERROR_TABLE INTO ERROR.
      CALL METHOD ERROR->RAISE_MESSAGE EXPORTING TYPE = 'E'.
    ENDLOOP.
    CLEAR ERROR_TABLE.

    PERFORM REFRESH_SALES.

    CREATE OBJECT DOCUMENT
              EXPORTING CONTROL = CONTROL
                        DOCUMENT_TYPE = DOCUMENT_TYPE.

  ENDIF.
  IF BDS_INSTANCE IS INITIAL.
    CREATE OBJECT BDS_INSTANCE.
  ENDIF.

ENDMODULE.                             " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: L_FCODE LIKE FCODE.

  L_FCODE = FCODE.
  CLEAR FCODE.

  CALL METHOD CL_GUI_CFW=>DISPATCH.

  CASE L_FCODE.
    WHEN 'EXIT'.                       "Zur?k
      IF NOT DOCUMENT IS INITIAL.
        CALL METHOD DOCUMENT->CLOSE_DOCUMENT.
        FREE DOCUMENT.
      ENDIF.
      IF NOT LINK_SERVER IS INITIAL.
        CALL METHOD LINK_SERVER->STOP_LINK_SERVER.
        FREE LINK_SERVER.
      ENDIF.
      IF NOT TABLE_COLL IS INITIAL.
        FREE TABLE_COLL.
      ENDIF.
      IF NOT CONTROL IS INITIAL.
        CALL METHOD CONTROL->DESTROY_CONTROL.
        FREE CONTROL.
      ENDIF.
      IF NOT BDS_INSTANCE IS INITIAL.
        FREE BDS_INSTANCE.
      ENDIF.

      LEAVE PROGRAM.
    WHEN 'CREATE'.
      IF NOT CONTROL IS INITIAL.
        CALL METHOD DOCUMENT->CREATE_DOCUMENT.
        CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.
      ENDIF.
    WHEN 'SELECT'.
      IF NOT CONTROL IS INITIAL.
        DATA: DOCUMENTS TYPE DOCUMENT_LIST.
        DATA: DESCR TYPE DOCUMENT_DESCR.

        CLEAR DOCUMENTS.
        DESCR-DOCUMENT_NAME = 'Car Sales Chart'(DO1).
        DESCR-DOCUMENT_ID = 'DEMOEXCELCHART1'.
        APPEND DESCR TO DOCUMENTS.

        CLEAR DOC_URL.
        CALL METHOD DOCUMENT->RETRIEVE_DOCUMENT
                    EXPORTING DOCUMENTS = DOCUMENTS
                    IMPORTING DOCUMENT_FORMAT = DOCUMENT_FORMAT
                              DOC_URL = DOC_URL.

        IF NOT DOC_URL IS INITIAL.
          CALL METHOD DOCUMENT->CLOSE_DOCUMENT.

          CALL METHOD DOCUMENT->OPEN_DOCUMENT_URL
                            EXPORTING OPEN_INPLACE = 'X'
                                      DOC_URL = DOC_URL
                            IMPORTING ERROR = ERROR.
          CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.
        ELSE.
          MESSAGE E010.
        ENDIF.
      ENDIF.
    WHEN 'OPEN'.
      IF DOCUMENT->DATA_SIZE NE 0.
        IF NOT CONTROL IS INITIAL.
          CALL METHOD DOCUMENT->OPEN_DOCUMENT
                            IMPORTING ERROR = ERROR.
          CALL METHOD ERROR->RAISE_MESSAGE EXPORTING TYPE = 'E'.
        ENDIF.
      ELSE.
        MESSAGE E005.
      ENDIF.
    WHEN 'INPLACE'.
      IF DOCUMENT->DATA_SIZE NE 0.
        IF NOT CONTROL IS INITIAL.
          CALL METHOD DOCUMENT->OPEN_DOCUMENT
                            EXPORTING OPEN_INPLACE = 'X'
                                      OPEN_READONLY = 'X'
                            IMPORTING ERROR = ERROR.
          CALL METHOD ERROR->RAISE_MESSAGE EXPORTING TYPE = 'E'.
        ENDIF.
      ELSE.
        MESSAGE E005.
      ENDIF.
    WHEN 'SAVEAS'.
      IF NOT DOCUMENT IS INITIAL AND NOT DOCUMENT->PROXY IS INITIAL.
        CALL METHOD DOCUMENT->PROXY->SAVE_AS
                          EXPORTING PROMPT_USER = 'X'
                          IMPORTING ERROR = ERROR.
        CALL METHOD ERROR->RAISE_MESSAGE EXPORTING TYPE = 'E'.
      ELSE.
        MESSAGE E000.
      ENDIF.
    WHEN 'CLOSE'.
      IF NOT DOCUMENT IS INITIAL.
        CALL METHOD DOCUMENT->CLOSE_DOCUMENT
                      EXPORTING DO_SAVE = 'X'
                      IMPORTING ERROR = ERROR.
        CALL METHOD ERROR->RAISE_MESSAGE EXPORTING TYPE = 'E'.
      ELSE.
        MESSAGE E000.
      ENDIF.
    WHEN 'COPYLINK'.
      IF NOT LINK_SERVER IS INITIAL.
        PERFORM REFRESH_SALES.
        CALL METHOD LINK_SERVER->EXECUTE_COPY_LINK_DIALOG
                 IMPORTING ERROR = ERROR.
        CALL METHOD ERROR->RAISE_MESSAGE EXPORTING TYPE = 'E'.
      ELSE.
        MESSAGE E004.
      ENDIF.
    WHEN 'REFRESH'.
      IF NOT LINK_SERVER IS INITIAL.
        PERFORM REFRESH_SALES.
      ELSE.
        MESSAGE E004.
      ENDIF.
      IF NOT DOCUMENT IS INITIAL AND NOT DOCUMENT->PROXY IS INITIAL.
        CALL METHOD DOCUMENT->PROXY->EXECUTE_MACRO
                      EXPORTING MACRO_STRING = 'R3StartupMacro'
                                PARAM_COUNT = 1
                                PARAM1 = 10
                      IMPORTING ERROR = ERROR.
*                      CHANGING  retvalue = usa_sales.
*         CALL METHOD c_oi_errors=>show_message EXPORTING type = 'E'.
      ELSE.
        MESSAGE E000.
      ENDIF.
    WHEN 'PRINT'.
      IF NOT DOCUMENT IS INITIAL AND NOT DOCUMENT->PROXY IS INITIAL.
        CALL METHOD DOCUMENT->PROXY->PRINT_DOCUMENT
                       EXPORTING PROMPT_USER = 'X'
                       IMPORTING ERROR = ERROR.
        CALL METHOD ERROR->RAISE_MESSAGE EXPORTING TYPE = 'E'.
      ELSE.
        MESSAGE E000.
      ENDIF.
  ENDCASE.
ENDMODULE.                             " USER_COMMAND_0100  INPUT

************************************************************************
*  CLASS c_excel_document IMPLEMENTATION.
************************************************************************
CLASS C_EXCEL_DOCUMENT IMPLEMENTATION.
  METHOD: CONSTRUCTOR.
*              IMPORTING control TYPE REF TO i_oi_container_control
*                        document_type TYPE soi_document_type
    ME->CONTROL = CONTROL.
    ME->DOCUMENT_TYPE = DOCUMENT_TYPE.
  ENDMETHOD.

  METHOD CREATE_DOCUMENT.
*                 IMPORTING open_inplace  TYPE c DEFAULT ' '
*                 EXPORTING error TYPE REF TO i_oi_error.
    IF NOT PROXY IS INITIAL.
      CALL METHOD ME->CLOSE_DOCUMENT.
    ENDIF.
    CALL METHOD CONTROL->GET_DOCUMENT_PROXY
             EXPORTING DOCUMENT_TYPE = DOCUMENT_TYPE
                       REGISTER_CONTAINER = 'X'
             IMPORTING DOCUMENT_PROXY = PROXY
                       ERROR = ERROR.
    IF ERROR->ERROR_CODE NE C_OI_ERRORS=>RET_OK.
      EXIT.
    ENDIF.

    CALL METHOD PROXY->CREATE_DOCUMENT
                        EXPORTING CREATE_VIEW_DATA = 'X'
                                  OPEN_INPLACE = OPEN_INPLACE
                                DOCUMENT_TITLE = 'R/3 Demo' "#EC NOTEXT
                        IMPORTING ERROR = ERROR.
    IF ERROR->ERROR_CODE NE C_OI_ERRORS=>RET_OK.
      EXIT.
    ENDIF.

    SET HANDLER ME->ON_CLOSE_DOCUMENT FOR PROXY.
    SET HANDLER ME->ON_CUSTOM_EVENT FOR PROXY.
  ENDMETHOD.

  METHOD OPEN_DOCUMENT.
*                 IMPORTING open_inplace  TYPE c DEFAULT ' '
*                           open_readonly TYPE c DEFAULT ' '
*                 EXPORTING error TYPE REF TO i_oi_error.
    IF NOT PROXY IS INITIAL.
      CALL METHOD ME->CLOSE_DOCUMENT.
    ENDIF.
    CALL METHOD CONTROL->GET_DOCUMENT_PROXY
             EXPORTING DOCUMENT_TYPE = DOCUMENT_TYPE
                       REGISTER_CONTAINER = 'X'
             IMPORTING DOCUMENT_PROXY = PROXY
                       ERROR = ERROR.
    IF ERROR->ERROR_CODE NE C_OI_ERRORS=>RET_OK.
      EXIT.
    ENDIF.

    CALL METHOD PROXY->OPEN_DOCUMENT_FROM_TABLE
                           EXPORTING DOCUMENT_TABLE = DATA_TABLE
                                     DOCUMENT_SIZE  = DATA_SIZE
                                     OPEN_INPLACE = OPEN_INPLACE
                                     OPEN_READONLY = OPEN_READONLY
                                     DOCUMENT_TITLE = 'R/3 Demo'
                           IMPORTING ERROR = ERROR.
    IF ERROR->ERROR_CODE NE C_OI_ERRORS=>RET_OK.
      EXIT.
    ENDIF.

    SET HANDLER ME->ON_CLOSE_DOCUMENT FOR PROXY.
    SET HANDLER ME->ON_CUSTOM_EVENT FOR PROXY.

    CALL METHOD PROXY->UPDATE_DOCUMENT_LINKS
                           IMPORTING ERROR = ERROR.
  ENDMETHOD.

  METHOD OPEN_DOCUMENT_URL.
*                 IMPORTING open_inplace  TYPE c DEFAULT ' '
*                           open_readonly TYPE c DEFAULT ' '
*                           doc_url TYPE t_url DEFAULT ' '
*                 EXPORTING error TYPE REF TO i_oi_error.
    DATA: SAVE_ERROR TYPE REF TO I_OI_ERROR.

    IF NOT PROXY IS INITIAL.
      CALL METHOD ME->CLOSE_DOCUMENT.
    ENDIF.
    CALL METHOD CONTROL->GET_DOCUMENT_PROXY
             EXPORTING DOCUMENT_TYPE = DOCUMENT_TYPE
                       REGISTER_CONTAINER = 'X'
             IMPORTING DOCUMENT_PROXY = PROXY
                       ERROR = ERROR.
    IF ERROR->ERROR_CODE NE C_OI_ERRORS=>RET_OK.
      EXIT.
    ENDIF.

    ME->DOC_URL = DOC_URL.
    CALL METHOD PROXY->OPEN_DOCUMENT
                           EXPORTING DOCUMENT_URL = DOC_URL
                                     OPEN_INPLACE = OPEN_INPLACE
                                     OPEN_READONLY = OPEN_READONLY
                           IMPORTING ERROR = ERROR.
    IF ERROR->ERROR_CODE NE C_OI_ERRORS=>RET_OK.
      EXIT.
    ENDIF.

*   Document shall also be available in ITAB for respective operations:
    CALL METHOD PROXY->SAVE_DOCUMENT_TO_TABLE
                      IMPORTING ERROR = SAVE_ERROR
                      CHANGING  DOCUMENT_TABLE = DATA_TABLE
                                DOCUMENT_SIZE = DATA_SIZE.
    IF NOT SAVE_ERROR IS INITIAL.
      IF SAVE_ERROR->ERROR_CODE NE C_OI_ERRORS=>RET_OK.
        ERROR = SAVE_ERROR.
        EXIT.
      ENDIF.
    ENDIF.
    SET HANDLER ME->ON_CLOSE_DOCUMENT FOR PROXY.
    SET HANDLER ME->ON_CUSTOM_EVENT FOR PROXY.

    CALL METHOD PROXY->UPDATE_DOCUMENT_LINKS
                           IMPORTING ERROR = ERROR.
  ENDMETHOD.

  METHOD RETRIEVE_DOCUMENT.
*              importing documents type document_list
*              exporting document_format type soi_document_format
*                        doc_url type t_url.
*------BDS-Data-Structures:---------------------------------------------

* Tables and WAs:
    DATA: DOC_SIGNATURE TYPE SBDST_SIGNATURE,
          WA_DOC_SIGNATURE LIKE LINE OF DOC_SIGNATURE,
          DOC_COMPONENTS TYPE SBDST_COMPONENTS,
          WA_DOC_COMPONENTS LIKE LINE OF DOC_COMPONENTS,
          DOC_PROPERTIES TYPE SBDST_PROPERTIES,
          WA_DOC_PROPERTIES LIKE LINE OF DOC_PROPERTIES,
          DOC_URIS TYPE SBDST_URI,
          WA_DOC_URIS LIKE LINE OF DOC_URIS.
* ID's:
    DATA: DOC_CLASSNAME TYPE SBDST_CLASSNAME VALUE 'SOFFICEINTEGRATION',
          DOC_CLASSTYPE TYPE SBDST_CLASSTYPE VALUE 'OT',
        DOC_OBJECT_KEY TYPE SBDST_OBJECT_KEY VALUE 'SOFFICEINTEGRATION',
          DOC_MIMETYPE TYPE BAPICOMPON-MIMETYPE.
*-----------------------------------------------------------------------

    DATA: FIELD_DESC TYPE TABLE OF RSVBFIDESC.
    DATA: WA_FIELD_DESC TYPE RSVBFIDESC.
    DATA: L_NR LIKE SY-TABIX.

    CLEAR: FIELD_DESC, WA_FIELD_DESC.
    WA_FIELD_DESC-FIELDNUM = 1.
    WA_FIELD_DESC-DISPLAY = 'X'.
    APPEND WA_FIELD_DESC TO FIELD_DESC.

    L_NR = 0.
    CALL FUNCTION 'RS_VALUES_BOX'
         EXPORTING
              LEFT_UPPER_COL = 5
              LEFT_UPPER_ROW = 5
              PAGESIZE       = 10
              TITLE          = 'Select document'(SDC)
         IMPORTING
              LINENUMBER     = L_NR
         TABLES
              FIELD_DESC     = FIELD_DESC
              VALUE_TAB      = DOCUMENTS
         EXCEPTIONS
              OTHERS         = 1.

    IF SY-SUBRC EQ 0 AND L_NR NE 0.
      READ TABLE DOCUMENTS INDEX L_NR INTO DESCR.
      IF SY-SUBRC EQ 0.
        CLEAR: WA_DOC_SIGNATURE, WA_DOC_COMPONENTS, WA_DOC_URIS.
        CLEAR: DOC_SIGNATURE, DOC_COMPONENTS, DOC_URIS.

        WA_DOC_SIGNATURE-PROP_NAME = 'DESCRIPTION'.
        WA_DOC_SIGNATURE-PROP_VALUE = DESCR-DOCUMENT_ID.
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

      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD CLOSE_DOCUMENT.
*                 IMPORTING do_save TYPE c DEFAULT ' '
*                 EXPORTING error TYPE REF TO i_oi_error.
    DATA: IS_CLOSED TYPE I, HAS_CHANGED TYPE I.
    DATA: SAVE_ERROR TYPE REF TO I_OI_ERROR.

    IF NOT PROXY IS INITIAL.
      CALL METHOD PROXY->IS_DESTROYED IMPORTING RET_VALUE = IS_CLOSED.

      IF IS_CLOSED IS INITIAL.
        CALL METHOD PROXY->CLOSE_DOCUMENT
                     EXPORTING DO_SAVE = DO_SAVE
                     IMPORTING HAS_CHANGED = HAS_CHANGED
                               ERROR = ERROR.
        IF ERROR->ERROR_CODE NE C_OI_ERRORS=>RET_OK.
          EXIT.
        ENDIF.
      ENDIF.

      IF NOT HAS_CHANGED IS INITIAL.
        CALL METHOD PROXY->SAVE_DOCUMENT_TO_TABLE
                      EXPORTING NO_FLUSH = 'X'
                      IMPORTING ERROR = SAVE_ERROR
                      CHANGING  DOCUMENT_TABLE = DATA_TABLE
                                DOCUMENT_SIZE = DATA_SIZE.
      ENDIF.

      CALL METHOD PROXY->RELEASE_DOCUMENT
                                   IMPORTING ERROR = ERROR.

      IF NOT SAVE_ERROR IS INITIAL.
        IF SAVE_ERROR->ERROR_CODE NE C_OI_ERRORS=>RET_OK.
          ERROR = SAVE_ERROR.
        ENDIF.
      ENDIF.

      SET HANDLER ME->ON_CLOSE_DOCUMENT FOR PROXY ACTIVATION ' '.
      SET HANDLER ME->ON_CUSTOM_EVENT FOR PROXY ACTIVATION ' '.
    ELSE.
      CALL METHOD C_OI_ERRORS=>CREATE_ERROR_FOR_RETCODE
              EXPORTING RETCODE = C_OI_ERRORS=>RET_DOCUMENT_NOT_OPEN
                        NO_FLUSH = ' '
              IMPORTING ERROR = ERROR.
    ENDIF.
  ENDMETHOD.

  METHOD ON_CLOSE_DOCUMENT.
*              FOR EVENT on_close_document OF c_oi_container_control
*              IMPORTING document_proxy has_changed.
    DATA: ANSWER, DO_SAVE.

    IF HAS_CHANGED EQ 1.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
           EXPORTING
                TITLEBAR              = 'Office Integration Demo'(OID)
                TEXT_QUESTION         = 'Save Document?'(SAV)
                DISPLAY_CANCEL_BUTTON = ' '
           IMPORTING
                ANSWER                = ANSWER.
      IF ANSWER EQ '1'.
        DO_SAVE = 'X'.
      ELSE.
        DO_SAVE = ' '.
      ENDIF.
      CALL METHOD ME->CLOSE_DOCUMENT
                    EXPORTING DO_SAVE = DO_SAVE
                    IMPORTING ERROR = ERROR.
      CALL METHOD ERROR->RAISE_MESSAGE
                                  EXPORTING TYPE = 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD ON_CUSTOM_EVENT.
*              FOR EVENT on_custom_event OF i_oi_document_proxy
*              IMPORTING document_proxy event_name param_count
*                        param1 param2 param3.
    IF EVENT_NAME EQ 'ON_SERIES_CHANGE'.
*      CALL METHOD TABLE_COLL->GET_TABLE
*                    EXPORTING TABLE_NAME = 'SALES_IN'
*                    IMPORTING ERROR = ERROR
*                    CHANGING  DATA_TABLE = TEST_TABLE.
*      CALL METHOD ERROR->RAISE_MESSAGE
*                                  EXPORTING TYPE = 'E'.
*      CLEAR WA_TEST_TABLE.
*      READ TABLE TEST_TABLE INTO WA_TEST_TABLE INDEX 1.
*      USA_SALES = WA_TEST_TABLE-SALES.
*      CLEAR WA_TEST_TABLE.
*      READ TABLE TEST_TABLE INTO WA_TEST_TABLE INDEX 2.
*      EUROPE_SALES = WA_TEST_TABLE-SALES.
*      CLEAR WA_TEST_TABLE.
*      READ TABLE TEST_TABLE INTO WA_TEST_TABLE INDEX 3.
*      JAPAN_SALES = WA_TEST_TABLE-SALES.
*      CLEAR WA_TEST_TABLE.
*      READ TABLE TEST_TABLE INTO WA_TEST_TABLE INDEX 4.
*      ASIA_SALES = WA_TEST_TABLE-SALES.
*      CLEAR WA_TEST_TABLE.
*      READ TABLE TEST_TABLE INTO WA_TEST_TABLE INDEX 5.
*      AMERICA_SALES = WA_TEST_TABLE-SALES.
*      CLEAR WA_TEST_TABLE.
*      READ TABLE TEST_TABLE INTO WA_TEST_TABLE INDEX 6.
*      AFRICA_SALES = WA_TEST_TABLE-SALES.
*      CLEAR WA_TEST_TABLE.

    ENDIF.
  ENDMETHOD.
ENDCLASS.

************************************************************************
* FORM refresh_sales.
************************************************************************
FORM REFRESH_SALES.
  PERFORM READ_DATA.

  CLEAR: IT_TEMP_MONBD[], IT_TEMP_ANBD[].

  DATA: WA_FIELD_NAME(30),
        WA_YEAR(4),
        WA_MONTH(10), "TYPE D,
        WA_COUNT(2) TYPE N.

  DATA: ERROR TYPE REF TO I_OI_ERROR.

  IF NOT LINK_SERVER IS INITIAL.

    IT_TEMP_MONBD[] = IT_MONBD[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Monthly'
                       ITEM_TITLE  = 'Monthly Brekdown Rate'
                       DDIC_NAME   = 'ZSPM_MONTH'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_MONBD.

    IT_TEMP_ANBD[] = IT_ANBD[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Annually'
                       ITEM_TITLE  = 'Annaully Reakdown Rate'
                       DDIC_NAME   = 'ZSPM_YEAR'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_ANBD.

    LOOP AT  IT_RANGE.
      MOVE SY-TABIX TO WA_COUNT.

      CONCATENATE 'Month' WA_COUNT INTO WA_FIELD_NAME.
      MOVE  IT_RANGE-ZMONTH TO WA_MONTH.
      CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME   = WA_FIELD_NAME
                        ITEM_VALUE  = WA_MONTH
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.
      CLEAR : WA_FIELD_NAME, WA_MONTH.
    ENDLOOP.

    READ TABLE IT_YEAR INDEX 1.
    MOVE: IT_YEAR-AJAHR TO WA_YEAR.
    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
                EXPORTING ITEM_NAME = 'Year_01'
                          ITEM_VALUE  = WA_YEAR
                          NO_FLUSH    = 'X'
                IMPORTING RETCODE = RETCODE.

    READ TABLE IT_YEAR INDEX 2.
    MOVE: IT_YEAR-AJAHR TO WA_YEAR.
    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
                EXPORTING ITEM_NAME = 'Year_02'
                          ITEM_VALUE  = WA_YEAR
                          NO_FLUSH    = 'X'
                IMPORTING RETCODE = RETCODE.

    READ TABLE IT_YEAR INDEX 3.
    MOVE: IT_YEAR-AJAHR TO WA_YEAR.
    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
                EXPORTING ITEM_NAME = 'Year_03'
                          ITEM_VALUE  = WA_YEAR
                          NO_FLUSH    = 'X'
                IMPORTING RETCODE = RETCODE.

    READ TABLE IT_YEAR INDEX 4.
    MOVE: IT_YEAR-AJAHR TO WA_YEAR.
    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
                EXPORTING ITEM_NAME = 'Year_04'
                          ITEM_VALUE  = WA_YEAR
                          NO_FLUSH    = 'X'
               IMPORTING RETCODE = RETCODE.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
  DATA: WA_YEAR      LIKE ZTPM_ANBD-AJAHR,
        WA_DATE      LIKE SY-DATUM,
        CAL_DATE     LIKE SY-DATUM,
        WA_MONTH(2)  TYPE N,
        WA_COUNT     LIKE T5A4A-DLYMO.

  CLEAR: IT_SHOP, IT_SHOP[], IT_YEAR, IT_YEAR[],
         IT_ZTPM_ANBD, IT_ZTPM_ANBD[],
         IT_ANBD, IT_ANBD[], IT_RANGE, IT_RANGE[],
         IT_MONBD, IT_MONBD[].

  SELECT DISTINCT SHOP  INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
          FROM  ZTPM_SHOP
          WHERE SHOP IN S_SHOP
          AND   SPRAS = SY-LANGU.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-002.
    LEAVE TO SCREEN 0.
  ENDIF.

  IT_YEAR-AJAHR = P_MONTH(4).
  APPEND IT_YEAR.
  IT_YEAR-AJAHR = P_MONTH(4) - 1.
  APPEND IT_YEAR.
  IT_YEAR-AJAHR = P_MONTH(4) - 2.
  APPEND IT_YEAR.
  IT_YEAR-AJAHR = P_MONTH(4) - 3.
  APPEND IT_YEAR.
  SORT  IT_YEAR BY AJAHR. "DESCENDING.


*** get Annually Breakdown rate...
  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZTPM_ANBD
           FROM  ZTPM_ANBD
           WHERE SHOP  IN S_SHOP
           AND   AJAHR BETWEEN WA_S_YEAR AND WA_E_YEAR.

  LOOP AT IT_SHOP.
    LOOP AT IT_YEAR.
      WA_YEAR = SY-TABIX.
      READ TABLE IT_ZTPM_ANBD WITH KEY SHOP = IT_SHOP-SHOP
                                       AJAHR = IT_YEAR-AJAHR.
      IF SY-SUBRC EQ 0.
        PERFORM SET_IT_ANBD USING WA_YEAR
                                  IT_ZTPM_ANBD-AVRATE.
      ENDIF.
    ENDLOOP.
    MOVE: IT_SHOP-SHOP TO IT_ANBD-SHOP.
    SELECT SINGLE SHTXT INTO IT_ANBD-SHTXT
                  FROM  ZTPM_SHOP
                  WHERE SHOP = IT_SHOP-SHOP
                  AND   SPRAS = SY-LANGU.
    APPEND IT_ANBD.
    CLEAR: WA_YEAR, IT_ANBD.
  ENDLOOP.

  SORT IT_ANBD BY SHOP.

*** make first day for select condition
  CONCATENATE  P_MONTH  '01' INTO WA_DATE.

*** make select entry (year/month)
  DO 12 TIMES.
    MOVE SY-INDEX TO WA_COUNT.
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
  ENDDO.

  SORT IT_RANGE BY ZMONTH ASCENDING.

  LOOP AT IT_SHOP.
    LOOP AT IT_RANGE.
      WA_MONTH = SY-TABIX.
*** Calculate Actual Monthly Breakdown rate...
      CALL FUNCTION 'Z_FPM_CAL_BREAKDOWN_RATE_MON'
           EXPORTING
                I_MONTH = IT_RANGE-ZMONTH
                I_SHOP  = IT_SHOP-SHOP
           TABLES
                T_RATE  = IT_RATE.

      READ TABLE IT_RATE INDEX 1.
      PERFORM SET_IT_MONBD USING WA_MONTH
                                 IT_RATE-AVRATE.
    ENDLOOP.
    MOVE: IT_SHOP-SHOP TO IT_MONBD-SHOP.
    APPEND IT_MONBD.
    CLEAR: WA_MONTH, IT_MONBD.
  ENDLOOP.
  SORT IT_MONBD BY SHOP.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_IT_ANBD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_YEAR  text
*      -->P_IT_ZTPM_ANBD_AVRATE  text
*----------------------------------------------------------------------*
FORM SET_IT_ANBD USING     P_YEAR
                          P_AVRATE.
  DATA: WA_YEAR LIKE FELD-NAME.

  CONCATENATE 'IT_ANBD-YEAR' P_YEAR INTO WA_YEAR.
  ASSIGN  (WA_YEAR) TO <YEAR>.
  MOVE  : P_AVRATE  TO <YEAR>.
ENDFORM.                    " SET_IT_ANBD
*&---------------------------------------------------------------------*
*&      Form  SET_IT_MONBD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MONTH  text
*      -->P_IT_RATE_AVRATE  text
*----------------------------------------------------------------------*
FORM SET_IT_MONBD USING    P_ZMONTH
                           P_AVRATE.
  DATA: WA_MONTH LIKE FELD-NAME.

  CONCATENATE 'IT_MONBD-MONTH' P_ZMONTH INTO WA_MONTH.
  ASSIGN  (WA_MONTH) TO <MONTH>.
  MOVE  : P_AVRATE  TO <MONTH>.

ENDFORM.                    " SET_IT_MONBD
