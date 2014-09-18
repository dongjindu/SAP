************************************************************************
* Program Name      : ZRPM05_TOP5
* Author            : Myoungho Park
* Creation Date     : 2003.09.17.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :  This Program is Monthly Breakdown Status
*                      & TOP-5 Report
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

REPORT  ZRPM05_TOP5   MESSAGE-ID DEMOOFFICEINTEGRATIO.

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

FIELD-SYMBOLS: <MONTH> .

CONSTANTS: C_LINE_COUNT TYPE I VALUE 5.

DATA: FIELDS_TABLE TYPE TABLE OF RFC_FIELDS.

TABLES: ZTPM_MONBD,  "//Monthly Breakdown Rate (Planed)
        ZSPM_BDMON,  "//average breakdown rate by Month
        ZSPM_MONTH,
        T357,
        ZSPM_PARAM.

DATA: IT_TEMP_MONBD LIKE ZSPM_MONTH2 OCCURS 0.
DATA: IT_MONBD LIKE ZSPM_MONTH2 OCCURS 0 WITH HEADER LINE.
DATA: IT_TEMP LIKE ZTPM_PLANDBD OCCURS 0 WITH HEADER LINE.

*** Monthly average breakdown rate
DATA: IT_RATE LIKE ZSPM_BDMON OCCURS 0 WITH HEADER LINE.

*** Top-5 internal table...
DATA: BEGIN OF IT_TOP5 OCCURS 0.
        INCLUDE STRUCTURE ZSPM_TOP5.
DATA: END OF IT_TOP5.

*** temp table
DATA: IT_TEMP_TOP5 LIKE IT_TOP5 OCCURS 0.

*** select entry
DATA: BEGIN OF IT_RANGE OCCURS 0,
         AJAHR  LIKE ZTPM_MONBD-AJAHR,
         ZMONTH LIKE ZTPM_MONBD-ZMONTH,
      END OF IT_RANGE.

DATA : P_ZMONTH LIKE ZSPM_PARAM-ZMONTH.
DATA : WA_MONTH LIKE ZSPM_PARAM-ZMONTH.

DATA: WA_SHOP_NAME LIKE ZTPM_SHOP-SHTXT,
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
*              FOR EVENT on_close_document OF c_oi_container_control
*              IMPORTING document_proxy has_changed.
    DATA: ANSWER.
*   if has_changed eq 1.
    PERFORM SAVE_DOCUMENT TABLES DATA_TABLE
                USING 'X' 'X'
                CHANGING DATA_SIZE DOCUMENT_PROXY RETCODE.
    OPEN_DOCUMENT = FALSE.
*   endif.
  ENDMETHOD.

  METHOD CUSTOM_EVENT_HANDLER.

  ENDMETHOD.
ENDCLASS.

*********** SELECTION-SCREEN ***********************************
****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT (5) TEXT-004.
PARAMETER : P_MONTH LIKE ZSPM_PARAM-MONTH DEFAULT SY-DATUM+4(2)
                                          OBLIGATORY .

SELECTION-SCREEN POSITION 30.
SELECTION-SCREEN COMMENT 26(5) TEXT-002.
PARAMETER : P_YEAR LIKE ZSPM_PARAM-AJAHR DEFAULT SY-DATUM(4)
                                         OBLIGATORY .

SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS : S_SHOP  FOR ZSPM_PARAM-SHOP  NO INTERVALS
                                              NO-EXTENSION
                                              OBLIGATORY.

*PARAMETER : P_PLACE LIKE ZSPM_PARAM-INPLACE DEFAULT 'X'
*                                            AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK BLOCK1.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_ZMONTH .
*  PERFORM SELECT_MONTH.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_YEAR.
  PERFORM SELECT_YEAR.
******************* INITIALIZATION ********************************
*******************************************************************
INITIALIZATION.

***************** AT SELECTION-SCREEN ******************************
********************************************************************
AT SELECTION-SCREEN.

  CONCATENATE P_YEAR P_MONTH INTO P_ZMONTH.

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
*      P_PLACE = 'X'.
      WA_INIT = 'X'.
      P_ZMONTH = SY-DATUM(6).
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
**** Create Container Control 'CONTAINER_02'
    CREATE OBJECT CONTAINER
              EXPORTING CONTAINER_NAME = 'CONTAINER_02'.
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
****  Link Server name SUFFIX : 'TOP5'
    CALL METHOD LINK_SERVER->START_LINK_SERVER
                      EXPORTING LINK_SERVER_MODE =
                                LINK_SERVER->LINK_SERVER_CUSTOMNAME
                                SERVER_NAME_SUFFIX = 'TOP5'
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
        WA_MONTH TYPE VVIFAELPER,
        WA_SELECT_MONTH TYPE STRING,
  WA_COUNT(2) TYPE N,
  WA_MONTH_YEAR(15).

*  MOVE: P_YEAR TO WA_YEAR .

  IF NOT LINK_SERVER IS INITIAL.
**** Link Monthly Brekdown Rate
    IT_TEMP_MONBD[] = IT_MONBD[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Monthly'
                       ITEM_TITLE  = 'Monthly Brekdown Rate'
                       DDIC_NAME   = 'ZSPM_MONTH2'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_MONBD.
*** Link Top-5
    IT_TEMP_TOP5[] = IT_TOP5[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Top5'
                       ITEM_TITLE  = 'Top5'
                       DDIC_NAME   = 'ZSPM_TOP5'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_TOP5.
**** Link Month
    LOOP AT  IT_RANGE.
      MOVE SY-TABIX TO WA_COUNT.

      CONCATENATE 'Month' WA_COUNT INTO WA_FIELD_NAME.
      CONCATENATE IT_RANGE-ZMONTH '/' IT_RANGE-AJAHR INTO WA_MONTH.
*      WRITE   IT_RANGE-ZMONTH  TO WA_MONTH USING EDIT MASK '____/__'.

      CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME   = WA_FIELD_NAME
                        ITEM_VALUE  = WA_MONTH
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.
      CLEAR : WA_FIELD_NAME, WA_MONTH.
    ENDLOOP.

**** report date
    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
      EXPORTING ITEM_NAME = 'report_date'
                    ITEM_VALUE  = SY-DATUM
                    NO_FLUSH    = 'X'
          IMPORTING RETCODE = RETCODE.

**** select date
    CONCATENATE '(' P_MONTH '/' P_YEAR ')' INTO WA_SELECT_MONTH.
    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
      EXPORTING ITEM_NAME = 'select_date'
                    ITEM_VALUE  = WA_SELECT_MONTH
                    NO_FLUSH    = 'X'
          IMPORTING RETCODE = RETCODE.

**** Link Year
    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
      EXPORTING ITEM_NAME = 'Year'
                    ITEM_VALUE  = WA_YEAR
                    NO_FLUSH    = 'X'
          IMPORTING RETCODE = RETCODE.

***** Link Shop name
    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
      EXPORTING ITEM_NAME = 'Shop'
                    ITEM_VALUE  = WA_SHOP_NAME
                    NO_FLUSH    = 'X'
          IMPORTING RETCODE = RETCODE.

**** Link month / year
    CLEAR: WA_MONTH_YEAR.
    CONCATENATE P_MONTH ' / ' P_YEAR INTO  WA_MONTH_YEAR.

    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
            EXPORTING ITEM_NAME   = WA_MONTH_YEAR
                      ITEM_VALUE  = 'Month_Year'
                      NO_FLUSH    = ' '
            IMPORTING RETCODE = RETCODE.

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
      PERFORM  FREE_OBJECT.
      LEAVE TO TRANSACTION SY-TCODE.

    WHEN 'EXCEL'.
      CLEAR: SY-UCOMM.
      IF S_SHOP-LOW IS INITIAL.
        MESSAGE E000(ZMPM) WITH TEXT-M01.
      ENDIF.
      CONCATENATE P_YEAR P_MONTH INTO P_ZMONTH.
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
        DOC_OBJECT_KEY TYPE SBDST_OBJECT_KEY VALUE 'TOP5',
        DOC_MIMETYPE LIKE BAPICOMPON-MIMETYPE.


  CLEAR DOC_URL.

  WA_DOC_SIGNATURE-PROP_NAME = 'DESCRIPTION'.
  WA_DOC_SIGNATURE-PROP_VALUE = 'Monthly Breakdown Status & TOP-5'.
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
    PERFORM SAVE_DOCUMENT TABLES  DATA_TABLE
                          USING   'X' 'X'
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
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
  DATA: WA_DATE      LIKE SY-DATUM,
        WA_FROM_DATE LIKE SY-DATUM,
        WA_TO_DATE   LIKE SY-DATUM,
        CAL_DATE     LIKE SY-DATUM,
        WA_COUNT     LIKE T5A4A-DLYMO,
        WA_MONTH(2)  TYPE N,
        WA_ZMONTH    LIKE ZSPM_PARAM-ZMONTH.

  CLEAR: IT_RANGE, IT_RANGE[],
         IT_TEMP,  IT_TEMP[],
         IT_MONBD, IT_MONBD[].

*** Get Shop name
*  SELECT SINGLE FING  INTO WA_SHOP_NAME
*                FROM  T357
*                WHERE WERKS = P_PLANT
*                AND   BEBER = P_SHOP.

  SELECT  SINGLE INNAM AS SHOP
          INTO  WA_SHOP_NAME
          FROM  T024I
          WHERE INGRP = S_SHOP-LOW.

*** make first day for select condition
  CONCATENATE P_ZMONTH '01' INTO WA_DATE.


*** make select entry (year/month)
  CLEAR : WA_COUNT.
  DO 12 TIMES.
*    MOVE SY-INDEX TO WA_COUNT.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
         EXPORTING
              DATE      = WA_DATE
              DAYS      = 0
              MONTHS    = WA_COUNT
              SIGNUM    = '-'
              YEARS     = 0
         IMPORTING
              CALC_DATE = CAL_DATE.
    MOVE : CAL_DATE(4)   TO IT_RANGE-AJAHR,
           CAL_DATE+4(2)   TO IT_RANGE-ZMONTH.
*    MOVE : CAL_DATE(6)   TO IT_RANGE-ZMONTH.
    APPEND IT_RANGE.
    MOVE SY-INDEX TO WA_COUNT.
  ENDDO.

  SORT IT_RANGE BY AJAHR ZMONTH ASCENDING.

*** Selection with Entry
  CLEAR: IT_TEMP, IT_TEMP[].
  SELECT *  APPENDING CORRESPONDING FIELDS OF TABLE IT_TEMP
            FROM ZTPM_PLANDBD
            FOR ALL ENTRIES IN IT_RANGE
            WHERE AJAHR   = IT_RANGE-AJAHR
            AND   ZMONTH  = IT_RANGE-ZMONTH
            AND   SHOP    = S_SHOP-LOW.
  IF SY-SUBRC EQ 0.        "//it_temp convert into flat month table
***                          (like : Month01, Month02, Month03,...)
    LOOP AT IT_TEMP.
      READ TABLE IT_RANGE WITH KEY AJAHR  = IT_TEMP-AJAHR
                                   ZMONTH = IT_TEMP-ZMONTH.
      WA_MONTH = SY-TABIX.
      PERFORM SET_IT_MONBD USING WA_MONTH
                                 IT_TEMP-AVRATE.
    ENDLOOP.
  ENDIF.
  APPEND IT_MONBD.
  CLEAR: IT_MONBD.


  LOOP AT IT_RANGE.
    WA_MONTH = SY-TABIX.
    CONCATENATE IT_RANGE-AJAHR IT_RANGE-ZMONTH INTO WA_ZMONTH.
*** Calculate Actual Monthly Breakdown rate...
    CALL FUNCTION 'Z_FPM_CAL_BREAKDOWN_RATE_MON'
         EXPORTING
              I_MONTH = WA_ZMONTH
              I_SHOP  = S_SHOP-LOW
              I_MAUEH = 'MIN'
         TABLES
              T_RATE  = IT_RATE.
    IF NOT IT_RATE IS INITIAL.      "//Convert into flat month table
***                        ( like : Month01, Month02, Month03,...)
      READ TABLE IT_RATE INDEX 1.
      PERFORM SET_IT_MONBD USING WA_MONTH
                                 IT_RATE-AVRATE.
    ENDIF.
  ENDLOOP.
  APPEND IT_MONBD.
  CLEAR: WA_MONTH, IT_MONBD.

*** Get Top5 data..
  CLEAR: IT_TOP5, IT_TOP5[].
  CALL FUNCTION 'Z_FPM_GET_TOP5'
       EXPORTING
            I_MONTH = P_ZMONTH
            I_SHOP  = S_SHOP-LOW
            I_MAUEH = 'MIN'
       TABLES
            T_TOP5  = IT_TOP5.

  PERFORM FILL_EMPTY_CELL.

ENDFORM.                    " READ_DATA
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

*  P_AVRATE = P_AVRATE * 100.

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
                                       OPEN_INPLACE = 'X'   "//P_PLACE
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
    P_ZMONTH = SPMON.
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
*&      Form  FILL_EMPTY_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_EMPTY_CELL.
  DATA: WA_LINES TYPE I.

  DO C_LINE_COUNT TIMES .
    DESCRIBE TABLE IT_TOP5 LINES WA_LINES.
    IF WA_LINES < C_LINE_COUNT.
      CLEAR IT_TOP5.
      APPEND IT_TOP5.
    ELSE.
      CLEAR: WA_LINES.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " FILL_EMPTY_CELL
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
*&      Module  INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_VALUE OUTPUT.
  IF P_MONTH IS INITIAL.
    P_MONTH = SY-DATUM+4(2).
  ENDIF.

  IF P_YEAR IS INITIAL.
    P_YEAR = SY-DATUM(4).
  ENDIF.

ENDMODULE.                 " INITIAL_VALUE  OUTPUT
