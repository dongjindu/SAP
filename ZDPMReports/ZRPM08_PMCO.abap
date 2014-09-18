************************************************************************
* Program Name      : ZRPM08_PMCO
* Author            : Myoungho Park
* Creation Date     : 2003.10.16.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :  This Program is Maintenance Cost Trend Report
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 2004.02.12. Myoungho Park
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


REPORT  ZRPM08_PMCO   MESSAGE-ID DEMOOFFICEINTEGRATIO.

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

FIELD-SYMBOLS: <MONTH> ,
               <FIELD>.

DATA: FIELDS_TABLE TYPE TABLE OF RFC_FIELDS.

TABLES: ZSPM_PARAM,
        ZTPM_PMCO,  "//Actual Maintenance Cost
        ZSPM_PMCO,  "//Maintenance cost structure
        ZTPM_PLANDCO.


DATA: IT_ZTPM_PLANDCO LIKE ZTPM_PLANDCO OCCURS 0 WITH HEADER LINE.
DATA: IT_ZTPM_PMCO LIKE ZTPM_PMCO OCCURS 0 WITH HEADER LINE.


DATA: IT_TEMP_PMCO LIKE ZSPM_MONTH3 OCCURS 0.
DATA: IT_PMCO LIKE ZSPM_MONTH3 OCCURS 0 WITH HEADER LINE.

DATA : IT_PREV_PMCO LIKE ZTPM_PMCO OCCURS 0 WITH HEADER LINE.
DATA : IT_PREV_PLANDCO  LIKE ZTPM_PLANDCO OCCURS 0 WITH HEADER LINE.


DATA: BEGIN OF IT_PREV OCCURS 0.
        INCLUDE STRUCTURE ZSPM_PREV.
DATA:  END OF IT_PREV.

DATA : IT_TEMP_PREV LIKE IT_PREV OCCURS 0.

DATA : BEGIN OF IT_AVERAGE OCCURS 0,
         VALUE TYPE WERTV8,
       END OF IT_AVERAGE.

DATA : IT_TEMP_AVERAGE LIKE IT_AVERAGE OCCURS 0.

DATA: BEGIN OF IT_SHOP OCCURS 0,
        SHOP  LIKE ZTPM_SHOP-SHOP,
        SHTXT LIKE ZTPM_SHOP-SHTXT,
      END OF IT_SHOP.

DATA: BEGIN OF IT_RANGE OCCURS 0,
         YEAR  LIKE ZSPM_PARAM-AJAHR,
         MONTH LIKE ZSPM_PARAM-MONTH,
      END OF IT_RANGE.

DATA : P_ZMONTH LIKE ZSPM_PARAM-ZMONTH.
DATA : WA_MONTH LIKE ZSPM_PARAM-MONTH.

DATA : WA_TITLE(50),
       WA_WERAS(5),
       WA_VALUE       TYPE WERTV8,
       WA_CURR_AMOUNT LIKE MLCD-LBKUM,
       WA_SUM_ACTUAL  TYPE WTGXXX.

DATA: WA_FIRST_DAY  LIKE SY-DATUM,
      WA_LAST_DAY  LIKE SY-DATUM,
      WA_FROM_DATE LIKE SY-DATUM,
      WA_TO_DATE   LIKE SY-DATUM,
      CAL_DATE     LIKE SY-DATUM,
      WA_COUNT     LIKE T5A4A-DLYMO.


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

*SELECTION-SCREEN BEGIN OF LINE.

*SELECTION-SCREEN COMMENT (5) TEXT-004.
*PARAMETER : P_MONTH LIKE ZSPM_PARAM-MONTH DEFAULT SY-DATUM+4(2).

*SELECTION-SCREEN POSITION 30.
*SELECTION-SCREEN COMMENT 26(5) TEXT-002.
PARAMETER : P_YEAR LIKE ZSPM_PARAM-AJAHR DEFAULT SY-DATUM(4).

*SELECTION-SCREEN END OF LINE.
*PARAMETER : P_PLACE LIKE ZSPM_PARAM-INPLACE DEFAULT 'X'
*                                            AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK BLOCK1.

******************* INITIALIZATION ********************************
*******************************************************************
INITIALIZATION.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_MONTH .
*  PERFORM SELECT_MONTH.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_YEAR.
  PERFORM SELECT_YEAR.

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
**** Create Container Control 'CONTAINER_05'
    CREATE OBJECT CONTAINER
              EXPORTING CONTAINER_NAME = 'CONTAINER_05'.
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
****  Link Server name SUFFIX : 'PMCO'
    CALL METHOD LINK_SERVER->START_LINK_SERVER
                     EXPORTING  LINK_SERVER_MODE =
                                LINK_SERVER->LINK_SERVER_CUSTOMNAME
                                SERVER_NAME_SUFFIX = 'PMCO'
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

  CONCATENATE 'Each Shop Maintenace Cost Trand-' P_YEAR 'Year'
              INTO WA_TITLE.

  IF NOT LINK_SERVER IS INITIAL.
*** Previous Year data...
    IT_TEMP_PREV[] = IT_PREV[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Prev_Year'
                       ITEM_TITLE  = 'Previous Year'
                       DDIC_NAME   = 'ZSPM_PREV'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_PREV.

    IT_TEMP_PMCO[] = IT_PMCO[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Current_Year'
                       ITEM_TITLE  = 'Current Year'
                       DDIC_NAME   = 'ZSPM_MONTH3'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_PMCO.

*    IT_TEMP_AVERAGE[] = IT_AVERAGE[].
*    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
*             EXPORTING ITEM_NAME   = 'Average'
*                       ITEM_TITLE  = 'Average'
*                       DDIC_NAME   = 'ZSPM_VALUE'
*                       NO_FLUSH    = 'X'
*             IMPORTING RETCODE = RETCODE
*             CHANGING  DATA_TABLE   = IT_TEMP_AVERAGE.

    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
            EXPORTING ITEM_NAME = 'report_date'
                      ITEM_VALUE  = SY-DATUM
                      NO_FLUSH    = 'X'
            IMPORTING RETCODE = RETCODE.

**** report title
    CONCATENATE TEXT-005 P_YEAR ')' INTO WA_TITLE.
    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
      EXPORTING ITEM_NAME = 'Title'
                    ITEM_VALUE  = WA_TITLE
                    NO_FLUSH    = 'X'
          IMPORTING RETCODE = RETCODE.


    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
         EXPORTING ITEM_NAME = 'Currency'
                   ITEM_VALUE  = WA_WERAS
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
      PERFORM FREE_OBJECT.
      LEAVE TO TRANSACTION SY-TCODE.

    WHEN 'EXCEL'.
      CLEAR: SY-UCOMM.
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
        DOC_OBJECT_KEY TYPE SBDST_OBJECT_KEY VALUE 'COST',
        DOC_MIMETYPE LIKE BAPICOMPON-MIMETYPE.

  CLEAR DOC_URL.

  WA_DOC_SIGNATURE-PROP_NAME  = 'DESCRIPTION'.
  WA_DOC_SIGNATURE-PROP_VALUE = 'Maintenance Cost Trend'.
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
  CLEAR : IT_PMCO, IT_PMCO[].
  CLEAR : IT_ZTPM_PLANDCO, IT_ZTPM_PLANDCO[].
  CLEAR : IT_ZTPM_PMCO, IT_ZTPM_PMCO[].

  SELECT  * INTO CORRESPONDING FIELDS OF TABLE IT_ZTPM_PLANDCO
            FROM ZTPM_PLANDCO
            WHERE AJAHR = P_YEAR.

  SELECT  * INTO CORRESPONDING FIELDS OF TABLE IT_ZTPM_PMCO
            FROM ZTPM_PMCO
            WHERE AJAHR = P_YEAR.

****  Get Planned Maintenance Costs ....
  LOOP AT IT_SHOP.
    LOOP AT IT_RANGE.
      WA_MONTH = SY-TABIX.
      CLEAR : WA_VALUE.
      PERFORM GET_PLANNED_MAINTEANCE_COST USING IT_SHOP-SHOP
                                                IT_RANGE-YEAR
                                                IT_RANGE-MONTH.

      PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                          WA_VALUE.
    ENDLOOP.
    APPEND IT_PMCO.  CLEAR IT_PMCO.

**** Get Actual Maintenance Costs ....
    LOOP AT IT_RANGE.
      WA_MONTH = SY-TABIX.
      CLEAR : WA_VALUE.

      PERFORM GET_ACTUAL_MAINTEANCE_COST USING IT_SHOP-SHOP
                                               IT_RANGE-YEAR
                                               IT_RANGE-MONTH.

      PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                          WA_VALUE.
    ENDLOOP.
    APPEND IT_PMCO. CLEAR IT_PMCO.

**** Get Planned Cost per Unit .....
    LOOP AT IT_RANGE.
      WA_MONTH = SY-TABIX.
      CLEAR : WA_VALUE.
      PERFORM GET_PLANNED_COST_PER_UNIT USING IT_SHOP-SHOP
                                              IT_RANGE-YEAR
                                              IT_RANGE-MONTH.

      PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                          WA_VALUE.
    ENDLOOP.
    APPEND IT_PMCO.  CLEAR IT_PMCO.

**** Get Actual Cost per Unit .....
    LOOP AT IT_RANGE.
      WA_MONTH = SY-TABIX.
      CLEAR : WA_VALUE.
      PERFORM GET_ACTUAL_COST_PER_UNIT USING IT_SHOP-SHOP
                                             IT_RANGE-YEAR
                                             IT_RANGE-MONTH.

      PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                          WA_VALUE.
    ENDLOOP.
    APPEND IT_PMCO. CLEAR IT_PMCO.

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

  CONCATENATE 'IT_PMCO-MONTH' P_MONTH INTO WA_MONTH.
  ASSIGN  (WA_MONTH) TO <MONTH>.
  MOVE  :  P_VALUE  TO <MONTH>.

ENDFORM.                    " SET_PLANNED_TASK_DATA
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
  CLEAR : IT_RANGE, IT_RANGE[].

  SELECT  DISTINCT INGRP AS SHOP
                   INNAM AS SHTXT
          INTO  CORRESPONDING FIELDS OF TABLE IT_SHOP
          FROM  T024I
          WHERE INGRP LIKE 'P%'
          ORDER BY SHOP.

*** make first day of select condition
  CONCATENATE P_YEAR '0101' INTO WA_FIRST_DAY.

  MOVE : WA_FIRST_DAY(4)     TO IT_RANGE-YEAR,
         WA_FIRST_DAY+4(2)   TO IT_RANGE-MONTH.
  APPEND IT_RANGE.

*** make select entries (year/month)
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

  SORT IT_RANGE BY YEAR MONTH ASCENDING.
ENDFORM.                    " set_select_entries
*&---------------------------------------------------------------------*
*&      Form  select_condition_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_CONDITION_DATA.

ENDFORM.                    " select_condition_data
*&---------------------------------------------------------------------*
*&      Form  READ_PREV_YEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PREV_YEAR_DATA.
  DATA : WA_PREV_YEAR LIKE ZSPM_PARAM-AJAHR.

  CLEAR : IT_PREV_PLANDCO, IT_PREV_PLANDCO[].
  CLEAR : IT_PREV_PMCO, IT_PREV_PMCO[].

  WA_PREV_YEAR = P_YEAR - 1.

*** previous year pland maintiance cost...
  SELECT   SHOP SUM( ZPCOST )  SUM( ZPUNITC )
            INTO CORRESPONDING FIELDS OF TABLE IT_PREV_PLANDCO
            FROM ZTPM_PLANDCO
            WHERE AJAHR = WA_PREV_YEAR
            GROUP BY SHOP AJAHR.

**** previous yaer actual mainteance cost...
  SELECT  SHOP SUM( ZACOST ) SUM( ZAUNITC )
            INTO CORRESPONDING FIELDS OF TABLE IT_PREV_PMCO
            FROM ZTPM_PMCO
            WHERE AJAHR = WA_PREV_YEAR
            GROUP BY SHOP AJAHR.

  LOOP AT IT_SHOP.
**** Planned Maintenance Cost
    READ TABLE IT_PREV_PLANDCO WITH KEY SHOP = IT_SHOP-SHOP.
    IF SY-SUBRC EQ 0.
      IT_PREV-VALUE = IT_PREV_PLANDCO-ZPCOST.
    ELSE.
      MOVE : ' ' TO IT_PREV-VALUE.
    ENDIF.
    APPEND IT_PREV.

**** Actual Maintenance Cost
    READ TABLE IT_PREV_PMCO WITH KEY SHOP = IT_SHOP-SHOP.
    IF SY-SUBRC EQ 0.
      IT_PREV-VALUE = IT_PREV_PMCO-ZACOST.
    ELSE.
      MOVE : ' ' TO IT_PREV-VALUE.
    ENDIF.
    APPEND IT_PREV.

*** Planned Cost per unit
    READ TABLE IT_PREV_PLANDCO WITH KEY SHOP = IT_SHOP-SHOP.
    IF SY-SUBRC EQ 0.
      IT_PREV-VALUE = IT_PREV_PLANDCO-ZPUNITC.
    ELSE.
      MOVE : ' ' TO IT_PREV-VALUE.
    ENDIF.
    APPEND IT_PREV.

*** Actual Cost per Unit
    READ TABLE IT_PREV_PMCO WITH KEY SHOP = IT_SHOP-SHOP.
    IF SY-SUBRC EQ 0.
      IT_PREV-VALUE = IT_PREV_PMCO-ZAUNITC.
    ELSE.
      MOVE : ' ' TO IT_PREV-VALUE.
    ENDIF.

    APPEND IT_PREV.
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
*  P_MONTH = SPMON.
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
*&      Form  GET_PLANNED_MAINTEANCE_COST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RANGE_MONTH  text
*      -->P_IT_SHOP_SHOP  text
*----------------------------------------------------------------------*
FORM GET_PLANNED_MAINTEANCE_COST USING   P_SHOP
                                         P_YEAR
                                         P_MONTH.

  READ TABLE IT_ZTPM_PLANDCO WITH KEY SHOP    = P_SHOP
                                      AJAHR   = P_YEAR
                                      ZMONTH  = P_MONTH.
  IF SY-SUBRC EQ 0.
    MOVE : IT_ZTPM_PLANDCO-ZPCOST TO WA_VALUE.
  ELSE.
    MOVE : ' ' TO WA_VALUE.
  ENDIF.
ENDFORM.                    " GET_PLANNED_MAINTEANCE_COST
*&---------------------------------------------------------------------*
*&      Form  GET_PLANNED_COST_PER_UNIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP_SHOP  text
*      -->P_IT_RANGE_YEAR  text
*      -->P_IT_RANGE_MONTH  text
*----------------------------------------------------------------------*
FORM GET_PLANNED_COST_PER_UNIT USING    P_SHOP
                                        P_YEAR
                                        P_MONTH.

  READ TABLE IT_ZTPM_PLANDCO WITH KEY SHOP    = P_SHOP
                                      AJAHR   = P_YEAR
                                      ZMONTH  = P_MONTH.
  IF SY-SUBRC EQ 0.
    MOVE : IT_ZTPM_PLANDCO-ZPUNITC TO WA_VALUE.
  ELSE.
    MOVE : ' ' TO WA_VALUE.
  ENDIF.
ENDFORM.                    " GET_PLANNED_COST_PER_UNIT
*&---------------------------------------------------------------------*
*&      Form  GET_ACTUAL_MAINTEANCE_COST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP_SHOP  text
*      -->P_IT_RANGE_YEAR  text
*      -->P_IT_RANGE_MONTH  text
*----------------------------------------------------------------------*
FORM GET_ACTUAL_MAINTEANCE_COST USING    P_SHOP
                                         P_YEAR
                                         P_MONTH.
  DATA : WA_MONTH LIKE SY-DATUM(6),
         WA_PREV_MONTH LIKE SY-DATUM(6).

  CONCATENATE  P_YEAR  P_MONTH INTO WA_MONTH.

  WA_PREV_MONTH = WA_MONTH - 1 .

  IF WA_MONTH EQ SY-DATUM(6) OR WA_MONTH EQ WA_PREV_MONTH.
*** Get Actual Value of Mainteance Cost...
    PERFORM GET_ACTUAL_COST USING P_SHOP
                                  P_YEAR
                                  P_MONTH.
    MOVE : WA_SUM_ACTUAL TO WA_VALUE.
  ELSE.
    READ TABLE IT_ZTPM_PMCO WITH KEY   SHOP    = P_SHOP
                                       AJAHR   = P_YEAR
                                       ZMONTH  = P_MONTH.
    IF SY-SUBRC EQ 0.
      MOVE : IT_ZTPM_PMCO-ZACOST TO WA_VALUE.
    ELSE.
      MOVE : ' ' TO WA_VALUE.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_ACTUAL_MAINTEANCE_COST
*&---------------------------------------------------------------------*
*&      Form  GET_ACTUAL_COST_PER_UNIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP_SHOP  text
*      -->P_IT_RANGE_YEAR  text
*      -->P_IT_RANGE_MONTH  text
*----------------------------------------------------------------------*
FORM GET_ACTUAL_COST_PER_UNIT USING    P_SHOP
                                       P_YEAR
                                       P_MONTH.
  DATA : WA_MONTH LIKE SY-DATUM(6),
         WA_PREV_MONTH LIKE SY-DATUM(6).

  CONCATENATE  P_YEAR  P_MONTH INTO WA_MONTH.

  WA_PREV_MONTH = WA_MONTH - 1 .

  IF WA_MONTH EQ SY-DATUM(6) OR WA_MONTH EQ WA_PREV_MONTH.

*** Get Amount of Produced Car...
    PERFORM GET_PRODUCED_CAR USING P_SHOP
                                   P_YEAR
                                   P_MONTH.

*** Get Actual Value of Mainteance Cost...
    PERFORM GET_ACTUAL_COST USING P_SHOP
                                  P_YEAR
                                  P_MONTH.
    IF NOT WA_CURR_AMOUNT IS INITIAL.
      WA_VALUE = WA_SUM_ACTUAL  / WA_CURR_AMOUNT.
    ELSE.
      MOVE : ' ' TO WA_VALUE.
    ENDIF.
  ELSE.
    READ TABLE IT_ZTPM_PMCO WITH KEY   SHOP    = P_SHOP
                                       AJAHR   = P_YEAR
                                       ZMONTH  = P_MONTH.
    IF SY-SUBRC EQ 0.
      MOVE : IT_ZTPM_PMCO-ZAUNITC TO WA_VALUE.
    ELSE.
      MOVE : ' ' TO WA_VALUE.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_ACTUAL_COST_PER_UNIT
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.

  PERFORM SET_SELECT_ENTRIES.

  PERFORM READ_PREV_YEAR_DATA.

  PERFORM READ_CONDITION_DATA.

*  PERFORM CAL_AVERAGE_DATA.

ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Module  SELECT_YEAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SELECT_YEAR INPUT.
  PERFORM SELECT_YEAR.
ENDMODULE.                 " SELECT_YEAR  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_ACTUAL_COST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_SHOP  text
*      -->P_P_YEAR  text
*      -->P_P_MONTH  text
*----------------------------------------------------------------------*
FORM GET_ACTUAL_COST USING    PP_SHOP
                              PP_YEAR
                              PP_MONTH.

  DATA : BEGIN OF IT_OBJECT OCCURS 0,
           SHKZG LIKE AUFM-SHKZG,
           DMBTR LIKE AUFM-DMBTR,
         END OF IT_OBJECT.

  DATA: WA_S_DAY LIKE SY-DATUM,
        WA_E_DAY LIKE SY-DATUM.

  DATA: BEGIN OF IT_OBJNR OCCURS 0,
          OBJNR LIKE AUFK-OBJNR,
          BUDAT LIKE AFRU-BUDAT,
        END OF IT_OBJNR.

  CLEAR: WA_SUM_ACTUAL.

  CONCATENATE PP_YEAR PP_MONTH INTO WA_S_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_S_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_E_DAY.

  SELECT  A~SHKZG A~DMBTR
          INTO CORRESPONDING FIELDS OF TABLE IT_OBJECT
          FROM AUFM AS A
               INNER JOIN VIAUFKST AS B
               ON  A~AUFNR = B~AUFNR
          WHERE A~BUDAT BETWEEN WA_S_DAY AND WA_E_DAY
          AND   B~AUART IN ('PM01', 'PM02', 'PM03', 'PM04')
          AND   B~INGPR = PP_SHOP.
  LOOP AT IT_OBJECT.
    IF IT_OBJECT-SHKZG  = 'S'.
      IT_OBJECT-DMBTR = IT_OBJECT-DMBTR * ( -1 ).
    ENDIF.

    WA_SUM_ACTUAL = WA_SUM_ACTUAL + IT_OBJECT-DMBTR.
  ENDLOOP.

ENDFORM.                    " GET_ACTUAL_COST
*&---------------------------------------------------------------------*
*&      Form  GET_PRODUCED_CAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_SHOP  text
*      -->P_P_YEAR  text
*      -->P_P_MONTH  text
*----------------------------------------------------------------------*
FORM GET_PRODUCED_CAR USING    PP_SHOP
                               PP_YEAR
                               PP_MONTH.

  CLEAR:  WA_CURR_AMOUNT.

  SELECT SINGLE ZUNIT INTO WA_CURR_AMOUNT
         FROM  ZTPM_PUNIT
         WHERE SHOP   = PP_SHOP
         AND   AJAHR  = PP_YEAR
         AND   ZMONTH = PP_MONTH.

ENDFORM.                    " GET_PRODUCED_CAR
*&---------------------------------------------------------------------*
*&      Form  CAL_AVERAGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_AVERAGE_DATA.
  DATA: WA_SUM TYPE DKGRKOSIST,
        WA_FIELD(20) TYPE C.

  LOOP AT IT_PMCO.
    CLEAR : WA_MONTH, WA_SUM.
    DO 12 TIMES.
      WA_MONTH = WA_MONTH + 1.
      IF P_YEAR = SY-DATUM(4) AND WA_MONTH > SY-DATUM+4(2).
        WA_MONTH = WA_MONTH - 1.
        EXIT.
      ELSE.
        CONCATENATE 'IT_PMCO-MONTH'  WA_MONTH INTO WA_FIELD.
        ASSIGN  (WA_FIELD) TO <FIELD>.
        WA_SUM  = WA_SUM   + <FIELD>.
      ENDIF.
    ENDDO.

    CLEAR  WA_VALUE .

    IF WA_MONTH NE 0.
      WA_VALUE = WA_SUM / WA_MONTH.
    ENDIF.

    MOVE WA_VALUE TO IT_AVERAGE-VALUE.

    APPEND IT_AVERAGE.
  ENDLOOP.
ENDFORM.                    " CAL_AVERAGE_DATA
