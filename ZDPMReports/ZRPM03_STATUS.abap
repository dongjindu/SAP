************************************************************************
* Program Name      : ZRPM03_STATUS
* Author            : Myoungho Park
* Creation Date     : 2003.10.22.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :  This Program is Month Each Shop Operation Status
*                      Report
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


REPORT  ZRPM03_STATUS   MESSAGE-ID DEMOOFFICEINTEGRATIO.

INCLUDE ZRPMOFFICE_TOP.

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


*---Global Variables & Tables

FIELD-SYMBOLS: <COLUMN> .

TABLES: ZTPM_OPTIME,
        ZTPM_MONBD,  "//Monthly Breakdown Rate (Planed)
        ZTPM_PLANDBD,
        ZSPM_BDMON,  "//average breakdown rate by Month
        ZTPM_BDNO,   "//Monthly  Actual No. of Breakdown
        ZTPM_PMCO,   "//Actual Maintenance Cost
        ZTPM_PLANDCO, "//Planned Maintenance Cost
        ZTPM_PMRO,   "//Maintenance Ratio
        ZSPM_MONTH,
        ZSPM_PARAM.

DATA: IT_PREV LIKE ZSPM_STATUS OCCURS 0 WITH HEADER LINE.
DATA: IT_TEMP_PREV LIKE ZSPM_STATUS OCCURS 0.

DATA: IT_TARGET LIKE ZSPM_STATUS OCCURS 0 WITH HEADER LINE.
DATA: IT_TEMP_TARGET LIKE ZSPM_STATUS OCCURS 0.

DATA: IT_ACTUAL LIKE ZSPM_STATUS OCCURS 0 WITH HEADER LINE.
DATA: IT_TEMP_ACTUAL LIKE ZSPM_STATUS OCCURS 0.

DATA: BEGIN OF IT_SHOP OCCURS 0,
         SHOP  LIKE T024I-INGRP,
      END OF IT_SHOP.

DATA : IT_PMROX LIKE ZTPM_PMRO OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_AUART OCCURS 0,
        AUART LIKE T003O-AUART,
      END OF IT_AUART.

DATA: WA_INIT,
      WA_RE_CALC, "// recalculation
      WA_ZMONTH LIKE ZSPM_PARAM-ZMONTH,        "//Period (month/year)
      WA_PREV_ZMONTH LIKE ZSPM_PARAM-ZMONTH,
      WA_VALUE  TYPE ZAVRATE, "(15) TYPE C,
      WA_AVRATE TYPE ZAVRATE,
      WA_INSP_VALUE(12) TYPE C,
      WA_PLAN_VALUE(12) TYPE C,
      WA_BREAK_VALUE(12) TYPE C,
      WA_PREV_YEAR LIKE ZSPM_PARAM-AJAHR,
      WA_PREV_MONTH LIKE ZSPM_PARAM-MONTH,
      WA_BDNUM  TYPE I,              "//No. of breakdown.
      WA_ZACTAL LIKE ZTPM_PMRO-ZACTAL,
      WA_ZPLAND LIKE ZTPM_PMRO-ZPLAND,
      WA_ZACOST LIKE ZTPM_PMCO-ZACOST,
      WA_ZPCOST LIKE ZTPM_PLANDCO-ZPCOST,
      WA_ZAUNITC LIKE ZTPM_PMCO-ZAUNITC,
      WA_ZPUNITC LIKE ZTPM_PLANDCO-ZPUNITC,
      WA_TITLE(50).

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
                                          OBLIGATORY.

SELECTION-SCREEN POSITION 30.
SELECTION-SCREEN COMMENT 26(5) TEXT-002.
PARAMETER : P_YEAR LIKE ZSPM_PARAM-AJAHR DEFAULT SY-DATUM(4)
                                         OBLIGATORY.

SELECTION-SCREEN END OF LINE.

*PARAMETER : P_PLACE LIKE ZSPM_PARAM-INPLACE DEFAULT 'X'
*                                            AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK BLOCK1.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_MONTH .
*  PERFORM SELECT_MONTH.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_YEAR.
  PERFORM SELECT_YEAR.
******************* INITIALIZATION ********************************
*******************************************************************
INITIALIZATION.

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
**** Create Container Control 'CONTAINER_06'
    CREATE OBJECT CONTAINER
              EXPORTING CONTAINER_NAME = 'CONTAINER_06'.
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
                                SERVER_NAME_SUFFIX = 'STATUS'
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

  DATA: WA_FIELD_NAME(30),
        WA_YEAR(4),
        WA_MONTH(7),
        WA_COUNT(2) TYPE N.

*  MOVE: P_YEAR TO WA_YEAR .

  IF NOT LINK_SERVER IS INITIAL.

    IT_TEMP_PREV[] = IT_PREV[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Prev'
                       ITEM_TITLE  = 'Prev M Actual'
                       DDIC_NAME   = 'ZSPM_STATUS'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_PREV.

    IT_TEMP_TARGET[] = IT_TARGET[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Target'
                       ITEM_TITLE  = 'Target'
                       DDIC_NAME   = 'ZSPM_STATUS'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_TARGET.

    IT_TEMP_ACTUAL[] = IT_ACTUAL[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Actual'
                       ITEM_TITLE  = 'Actual'
                       DDIC_NAME   = 'ZSPM_STATUS'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_ACTUAL.

    CONCATENATE  P_MONTH '/' P_YEAR ')'  INTO WA_TITLE.
    CONCATENATE TEXT-005  WA_TITLE  INTO WA_TITLE.

    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
      EXPORTING ITEM_NAME       = 'Title'
                    ITEM_VALUE  = WA_TITLE
                    NO_FLUSH    = 'X'
          IMPORTING RETCODE = RETCODE.

    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
      EXPORTING ITEM_NAME       = 'Report_date'
                    ITEM_VALUE  = SY-DATUM
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
        DOC_OBJECT_KEY TYPE SBDST_OBJECT_KEY VALUE 'STATUS',
        DOC_MIMETYPE LIKE BAPICOMPON-MIMETYPE.


  CLEAR DOC_URL.

  WA_DOC_SIGNATURE-PROP_NAME = 'DESCRIPTION'.
  WA_DOC_SIGNATURE-PROP_VALUE = 'Each Shop Operation Status'.
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
  DATA : WA_DATE  LIKE SY-DATUM,
         CAL_DATE LIKE SY-DATUM.

*** Clear internal tables...
  CLEAR : IT_SHOP, IT_SHOP[].
  CLEAR : IT_PREV, IT_PREV[].
  CLEAR : IT_TARGET, IT_TARGET[].
  CLEAR : IT_ACTUAL, IT_ACTUAL[].

  CONCATENATE P_YEAR P_MONTH INTO WA_ZMONTH.
  CONCATENATE P_YEAR P_MONTH '01' INTO WA_DATE.

*** Get previous month
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            DATE      = WA_DATE
            DAYS      = 0
            MONTHS    = 1
            SIGNUM    = '-'
            YEARS     = 0
       IMPORTING
            CALC_DATE = CAL_DATE.

  MOVE : CAL_DATE(4)   TO WA_PREV_YEAR,
         CAL_DATE+4(2) TO WA_PREV_MONTH,
         CAL_DATE(6)   TO WA_PREV_ZMONTH.

*** make Shop code selection entries
  SELECT  DISTINCT INGRP AS SHOP
                   INNAM AS SHTXT
          INTO  CORRESPONDING FIELDS OF TABLE IT_SHOP
          FROM  T024I
          WHERE INGRP LIKE 'P%'
          ORDER BY SHOP.

  PERFORM CHECK_PERIOD.

  PERFORM GET_BREAKDOWN_RATIO.
  PERFORM GET_MTTR.
  PERFORM GET_MTBF.
  PERFORM GET_PREVENTIVE_MAINTENANCE.
  PERFORM GET_BREAKDOWN_MAINTENANCE.
  PERFORM GET_MAINTENANCE_COST.
  PERFORM GET_COST_PER_UNIT.

ENDFORM.                    " READ_DATA

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
                                       OPEN_INPLACE = 'X'  "//P_PLACE
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
    ZSPM_PARAM-ZMONTH = SPMON.
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
*&      Form  CHECK_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_PERIOD.
  DATA : WA_PP_YEAR   LIKE ZSPM_PARAM-AJAHR,
         WA_PP_MONTH  LIKE ZSPM_PARAM-MONTH,
         CAL_DATE     LIKE SY-DATUM.

  CLEAR : WA_RE_CALC.

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

  IF ( P_YEAR = WA_PP_YEAR AND
       P_MONTH > WA_PP_MONTH ) OR
                     ( P_YEAR > WA_PP_YEAR ).
    MOVE 'X'  TO WA_RE_CALC.
  ELSE.

  ENDIF.
ENDFORM.                    " CHECK_PERIOD
*&---------------------------------------------------------------------*
*&      Form  GET_BREAKDOWN_RATIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BREAKDOWN_RATIO.
  IF WA_RE_CALC EQ SPACE.
    PERFORM READ_BREAKDOWN_RATIO.
  ELSE.
    PERFORM RE_CALC_BREAKDOWN_RATIO.
  ENDIF.
ENDFORM.                    " GET_BREAKDOWN_RATIO
*&---------------------------------------------------------------------*
*&      Form  GET_MTTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MTTR.
  IF WA_RE_CALC EQ SPACE.
    PERFORM READ_MTTR.
  ELSE.
    PERFORM RE_CALC_MTTR.
  ENDIF.
ENDFORM.                    " GET_MTTR
*&---------------------------------------------------------------------*
*&      Form  GET_MTBF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MTBF.
  IF WA_RE_CALC EQ SPACE.
    PERFORM READ_MTBF.
  ELSE.
    PERFORM RE_CALC_MTBF.
  ENDIF.
ENDFORM.                    " GET_MTBF
*&---------------------------------------------------------------------*
*&      Form  GET_PREVENTIVE_MAINTENANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PREVENTIVE_MAINTENANCE.
  IF WA_RE_CALC EQ SPACE.
    PERFORM READ_PREVENTIVE_MAINTENANCE.
  ELSE.
    PERFORM RE_CALC_PREVENTIVE_MAINTENANCE.
  ENDIF.
ENDFORM.                    " GET_PREVENTIVE_MAINTENANCE
*&---------------------------------------------------------------------*
*&      Form  GET_BREAKDOWN_MAINTENANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BREAKDOWN_MAINTENANCE.
  IF WA_RE_CALC EQ SPACE.
    PERFORM READ_BREAKDOWN_MAINTENANCE.
  ELSE.
    PERFORM RE_CALC_BREAKDOWN_MAINTENANCE.
  ENDIF.
ENDFORM.                    " GET_BREAKDOWN_MAINTENANCE
*&---------------------------------------------------------------------*
*&      Form  GET_MAINTENANCE_COST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MAINTENANCE_COST.
  IF WA_RE_CALC EQ SPACE.
    PERFORM READ_MAINTENANCE_COST.
  ELSE.
    PERFORM RE_CALC_MAINTENANCE_COST.
  ENDIF.
ENDFORM.                    " GET_MAINTENANCE_COST
*&---------------------------------------------------------------------*
*&      Form  GET_COST_PER_UNIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_COST_PER_UNIT.
  IF WA_RE_CALC EQ SPACE.
    PERFORM READ_COST_PER_UNIT.
  ELSE.
    PERFORM RE_CALC_COST_PER_UNIT.
  ENDIF.
ENDFORM.                    " GET_COST_PER_UNIT
*&---------------------------------------------------------------------*
*&      Form  READ_BREAKDOWN_RATIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_BREAKDOWN_RATIO.
**** Prev.Month Actual.
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, ZTPM_MONBD, ZTPM_OPTIME.
    SELECT SINGLE  *
           FROM  ZTPM_MONBD
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = WA_PREV_YEAR
           AND   ZMONTH = WA_PREV_MONTH.
    IF SY-SUBRC EQ 0.
      SELECT  SINGLE *
              FROM  ZTPM_OPTIME
              WHERE SHOP   = IT_SHOP-SHOP
              AND   AJAHR  = WA_PREV_YEAR
              AND   ZMONTH = WA_PREV_MONTH.
      IF NOT ZTPM_OPTIME-OPTIME IS INITIAL.
        WA_VALUE = ZTPM_MONBD-ZDOWNTIME / ZTPM_OPTIME-OPTIME * 100.
      ENDIF.
    ENDIF.

    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                              WA_VALUE.

  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.

**** Target
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, ZTPM_MONBD, ZTPM_OPTIME.
    SELECT SINGLE  *
           FROM  ZTPM_PLANDBD
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH.
    IF SY-SUBRC EQ 0.
      SELECT  SINGLE *
              FROM  ZTPM_OPTIME
              WHERE SHOP   = IT_SHOP-SHOP
              AND   AJAHR  = P_YEAR
              AND   ZMONTH = P_MONTH.
      IF NOT ZTPM_OPTIME-OPTIME IS INITIAL.
        WA_VALUE = ZTPM_MONBD-ZDOWNTIME / ZTPM_OPTIME-OPTIME * 100.
      ENDIF.
    ENDIF.

    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

**** Actual
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, ZTPM_MONBD, ZTPM_OPTIME.
    SELECT SINGLE  *
           FROM  ZTPM_MONBD
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH.
    IF SY-SUBRC EQ 0.
      SELECT  SINGLE *
              FROM  ZTPM_OPTIME
              WHERE SHOP   = IT_SHOP-SHOP
              AND   AJAHR  = P_YEAR
              AND   ZMONTH = P_MONTH.
      IF NOT ZTPM_OPTIME-OPTIME IS INITIAL.
        WA_VALUE = ZTPM_MONBD-ZDOWNTIME / ZTPM_OPTIME-OPTIME * 100.
      ENDIF.
    ENDIF.
    PERFORM SET_IT_ACTUAL USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.

ENDFORM.                    " READ_BREAKDOWN_RATIO
*&---------------------------------------------------------------------*
*&      Form  RE_CALC_BREAKDOWN_RATIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_CALC_BREAKDOWN_RATIO.
  DATA: IT_TEMP_RATE LIKE ZSPM_BDMON OCCURS 0 WITH HEADER LINE.

**** Prev.Month Actual.
  LOOP AT IT_SHOP.
    CLEAR: IT_TEMP_RATE, IT_TEMP_RATE[].

    CALL FUNCTION 'Z_FPM_CAL_BREAKDOWN_RATE_MON'
         EXPORTING
              I_MONTH = WA_PREV_ZMONTH
              I_SHOP  = IT_SHOP-SHOP
              I_MAUEH = 'MIN'
         TABLES
              T_RATE  = IT_TEMP_RATE.

    READ TABLE IT_TEMP_RATE INDEX 1.
    IF SY-SUBRC EQ 0.
      MOVE: IT_TEMP_RATE-AVRATE TO WA_VALUE.
      PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                                  WA_VALUE.
    ENDIF.
  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.

**** Target
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, ZTPM_MONBD, ZTPM_OPTIME.
    SELECT SINGLE  *
           FROM  ZTPM_PLANDBD
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH.
    IF SY-SUBRC EQ 0.
      SELECT  SINGLE *
              FROM  ZTPM_OPTIME
              WHERE SHOP   = IT_SHOP-SHOP
              AND   AJAHR  = P_YEAR
              AND   ZMONTH = P_MONTH.
      IF NOT ZTPM_OPTIME-OPTIME IS INITIAL.
        WA_VALUE = ZTPM_MONBD-ZDOWNTIME / ZTPM_OPTIME-OPTIME * 100.
      ENDIF.
    ENDIF.
    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.
  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

**** Actual
  LOOP AT IT_SHOP.
    CLEAR: IT_TEMP_RATE, IT_TEMP_RATE[].

    CALL FUNCTION 'Z_FPM_CAL_BREAKDOWN_RATE_MON'
         EXPORTING
              I_MONTH = WA_ZMONTH
              I_SHOP  = IT_SHOP-SHOP
              I_MAUEH = 'MIN'
         TABLES
              T_RATE  = IT_TEMP_RATE.

    READ TABLE IT_TEMP_RATE INDEX 1.
    IF SY-SUBRC EQ 0.
      MOVE: IT_TEMP_RATE-AVRATE TO WA_VALUE.
      PERFORM SET_IT_ACTUAL USING IT_SHOP-SHOP
                                  WA_VALUE.
    ENDIF.
  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.

ENDFORM.                    " RE_CALC_BREAKDOWN_RATIO
*&---------------------------------------------------------------------*
*&      Form  SET_IT_PREV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP_SHOP  text
*      -->P_WA_VALUE  text
*----------------------------------------------------------------------*
FORM SET_IT_PREV USING    P_SHOP
                          P_VALUE.
  DATA: WA_COLUMN LIKE FELD-NAME.
  IF P_VALUE = SPACE.
    P_VALUE = 0.
  ENDIF.
  CONCATENATE 'IT_PREV-' P_SHOP INTO WA_COLUMN.
  ASSIGN  (WA_COLUMN) TO <COLUMN>.
  MOVE  : P_VALUE  TO <COLUMN>.
ENDFORM.                    " SET_IT_PREV
*&---------------------------------------------------------------------*
*&      Form  SET_IT_TARGET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP_SHOP  text
*      -->P_WA_VALUE  text
*----------------------------------------------------------------------*
FORM SET_IT_TARGET USING    P_SHOP
                            P_VALUE.
  DATA: WA_COLUMN LIKE FELD-NAME.

  IF P_VALUE = SPACE.
    P_VALUE = 0.
  ENDIF.

  CONCATENATE 'IT_TARGET-' P_SHOP INTO WA_COLUMN.
  ASSIGN  (WA_COLUMN) TO <COLUMN>.
  MOVE  : P_VALUE  TO <COLUMN>.
ENDFORM.                    " SET_IT_TARGET
*&---------------------------------------------------------------------*
*&      Form  SET_IT_ACTUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP_SHOP  text
*      -->P_WA_VALUE  text
*----------------------------------------------------------------------*
FORM SET_IT_ACTUAL USING    P_SHOP
                            P_VALUE.
  DATA: WA_COLUMN LIKE FELD-NAME.
  IF P_VALUE = SPACE.
    P_VALUE = 0.
  ENDIF.
  CONCATENATE 'IT_ACTUAL-' P_SHOP INTO WA_COLUMN.
  ASSIGN  (WA_COLUMN) TO <COLUMN>.
  MOVE  : P_VALUE  TO <COLUMN>.
ENDFORM.                    " SET_IT_ACTUAL
*&---------------------------------------------------------------------*
*&      Form  READ_MTTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MTTR.
**** Prev.Month Actual.
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, ZTPM_MONBD, ZTPM_BDNO.
    SELECT SINGLE *
           FROM   ZTPM_MONBD
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = WA_PREV_YEAR
           AND    ZMONTH = WA_PREV_MONTH.
    SELECT SINGLE *
           FROM   ZTPM_BDNO
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = WA_PREV_YEAR
           AND    ZMONTH = WA_PREV_MONTH.
    IF NOT ZTPM_BDNO-ZBDNUM IS INITIAL.
      WA_VALUE = ZTPM_MONBD-ZDOWNTIME / ZTPM_BDNO-ZBDNUM.
    ENDIF.
    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                              WA_VALUE.

  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.

**** Target
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_AVRATE, ZTPM_MONBD, ZTPM_OPTIME.
    SELECT SINGLE  AVRATE  INTO  WA_AVRATE
           FROM  ZTPM_MTBT
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH
           AND   ZMTBT  = 'MTTR'.

    MOVE : WA_AVRATE  TO WA_VALUE.
    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

**** Actual
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, ZTPM_MONBD, ZTPM_BDNO.
    SELECT SINGLE  *
           FROM  ZTPM_MONBD
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH.
    IF SY-SUBRC EQ 0.
      SELECT  SINGLE *
              FROM  ZTPM_BDNO
              WHERE SHOP   = IT_SHOP-SHOP
              AND   AJAHR  = P_YEAR
              AND   ZMONTH = P_MONTH.
      IF NOT ZTPM_BDNO-ZBDNUM IS INITIAL.
        WA_VALUE = ZTPM_MONBD-ZDOWNTIME / ZTPM_BDNO-ZBDNUM.
      ENDIF.
    ENDIF.
    PERFORM SET_IT_ACTUAL USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.

ENDFORM.                    " READ_MTTR
*&---------------------------------------------------------------------*
*&      Form  RE_CALC_MTTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_CALC_MTTR.
  DATA : IT_BDTIME LIKE ZSPM_BDTIME OCCURS 0 WITH HEADER LINE.
  DATA :  WA_S_DAY     LIKE SY-DATUM,
          WA_E_DAY     LIKE SY-DATUM.

**** Prev.Month Actual.
*** MAKE SELECTION ENTRY...
  CONCATENATE WA_PREV_YEAR WA_PREV_MONTH '01' INTO WA_S_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_S_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_E_DAY.

  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, IT_BDTIME, IT_BDTIME[].

*** Get No of breakdown
    PERFORM GET_NO_BREAKDOWN USING IT_SHOP-SHOP
                                   WA_S_DAY
                                   WA_E_DAY.

**** Sum of Downtime
    CALL FUNCTION 'Z_FPM_CAL_BREAKDOWN_TIME_MON'
         EXPORTING
              I_MONTH = WA_PREV_ZMONTH
              I_SHOP  = IT_SHOP-SHOP
              I_MAUEH = 'MIN'
         TABLES
              T_TIME  = IT_BDTIME.

    READ TABLE IT_BDTIME INDEX 1.
    IF SY-SUBRC EQ 0.
      IF WA_BDNUM  NE 0.
        WA_VALUE =  IT_BDTIME-ZDOWNTIME / WA_BDNUM.
      ENDIF.
    ENDIF.
    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                              WA_VALUE.

  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.


**** Target
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_AVRATE, ZTPM_MONBD, ZTPM_OPTIME.

    SELECT SINGLE  AVRATE  INTO WA_AVRATE
           FROM  ZTPM_MTBT
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH
           AND   ZMTBT  = 'MTTR'.

    MOVE : WA_AVRATE  TO WA_VALUE.

    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

**** Actual
*** MAKE SELECTION ENTRY...
  CONCATENATE P_YEAR P_MONTH '01' INTO WA_S_DAY.
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_S_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_E_DAY.

  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, IT_BDTIME, IT_BDTIME[].
*** Get No of breakdown
    PERFORM GET_NO_BREAKDOWN USING IT_SHOP-SHOP
                                   WA_S_DAY
                                   WA_E_DAY.

**** Sum of Downtime
    CALL FUNCTION 'Z_FPM_CAL_BREAKDOWN_TIME_MON'
         EXPORTING
              I_MONTH = WA_ZMONTH
              I_SHOP  = IT_SHOP-SHOP
              I_MAUEH = 'MIN'
         TABLES
              T_TIME  = IT_BDTIME.

    READ TABLE IT_BDTIME INDEX 1.
    IF SY-SUBRC EQ 0.
      IF WA_BDNUM  NE 0.
        WA_VALUE =  IT_BDTIME-ZDOWNTIME / WA_BDNUM.
      ENDIF.
    ENDIF.
    PERFORM SET_IT_ACTUAL USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.

ENDFORM.                    " RE_CALC_MTTR
*&---------------------------------------------------------------------*
*&      Form  GET_NO_BREAKDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_S_DAY  text
*      -->P_WA_E_DAY  text
*----------------------------------------------------------------------*
FORM GET_NO_BREAKDOWN USING    P_SHOP
                               P_S_DAY
                               P_E_DAY.
  CLEAR: WA_BDNUM.
  CASE P_SHOP.
    WHEN 'P10'.
      SELECT  COUNT( DISTINCT A~AUFNR )
             INTO WA_BDNUM
             FROM AFRU  AS A
                  INNER JOIN VIQMEL AS B
                  ON A~AUFNR = B~AUFNR
             WHERE B~INGRP = P_SHOP
             AND   A~AUERU EQ 'X'
             AND   A~STOKZ EQ ' '
             AND   A~STZHL EQ SPACE
             AND   B~STORT IN ('PS1', 'PS2')
             AND   B~AUSVN BETWEEN  P_S_DAY    "//Start of Malfunction
                               AND  P_E_DAY.
    WHEN 'P20' OR 'P30' OR 'P40'.
      SELECT  COUNT( DISTINCT A~AUFNR )
             INTO WA_BDNUM
             FROM AFRU  AS A
                  INNER JOIN VIQMEL AS B
                  ON A~AUFNR = B~AUFNR
             WHERE B~INGRP = P_SHOP
             AND   A~AUERU EQ 'X'
             AND   A~STOKZ EQ ' '
             AND   A~STZHL EQ SPACE
             AND   B~AUSVN BETWEEN  P_S_DAY    "//Start of Malfunction
                               AND  P_E_DAY.
    WHEN 'P50'.
      SELECT  COUNT( DISTINCT A~AUFNR )
         INTO WA_BDNUM
         FROM AFRU  AS A
              INNER JOIN VIQMEL AS B
              ON A~AUFNR = B~AUFNR
         WHERE B~INGRP = P_SHOP
         AND   A~AUERU EQ 'X'
         AND   A~STOKZ EQ ' '
         AND   A~STZHL EQ SPACE
         AND   B~STORT  = 'V00'
         AND   B~AUSVN BETWEEN  P_S_DAY    "//Start of Malfunction
                           AND  P_E_DAY.
  ENDCASE.

ENDFORM.                    " GET_NO_BREAKDOWN
*&---------------------------------------------------------------------*
*&      Form  READ_MTBF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MTBF.
**** Prev.Month Actual.
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, ZTPM_OPTIME, ZTPM_BDNO.
    SELECT SINGLE *
           FROM   ZTPM_OPTIME
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = WA_PREV_YEAR
           AND    ZMONTH = WA_PREV_MONTH.
    SELECT SINGLE *
           FROM   ZTPM_BDNO
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = WA_PREV_YEAR
           AND    ZMONTH = WA_PREV_MONTH.
    IF NOT ZTPM_BDNO-ZBDNUM IS INITIAL.
      WA_VALUE = ZTPM_OPTIME-OPTIME / ZTPM_BDNO-ZBDNUM.
    ENDIF.
    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                              WA_VALUE.

  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.

**** Target
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_AVRATE, ZTPM_MONBD, ZTPM_OPTIME.
    SELECT SINGLE  AVRATE  INTO  WA_AVRATE
           FROM  ZTPM_MTBT
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH
           AND   ZMTBT  = 'MTBF'.

    MOVE : WA_AVRATE  TO WA_VALUE.
    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

**** Actual
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, ZTPM_OPTIME, ZTPM_BDNO.
    SELECT SINGLE  *
           FROM  ZTPM_OPTIME
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH.

    SELECT  SINGLE *
            FROM  ZTPM_BDNO
            WHERE SHOP   = IT_SHOP-SHOP
            AND   AJAHR  = P_YEAR
            AND   ZMONTH = P_MONTH.
    IF NOT ZTPM_BDNO-ZBDNUM IS INITIAL.
      WA_VALUE = ZTPM_OPTIME-OPTIME / ZTPM_BDNO-ZBDNUM.
    ENDIF.

    PERFORM SET_IT_ACTUAL USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.

ENDFORM.                    " READ_MTBF
*&---------------------------------------------------------------------*
*&      Form  RE_CALC_MTBF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_CALC_MTBF.
  DATA : IT_BDTIME LIKE ZSPM_BDTIME OCCURS 0 WITH HEADER LINE.
  DATA :  WA_S_DAY     LIKE SY-DATUM,
          WA_E_DAY     LIKE SY-DATUM.

**** Prev.Month Actual.
*** MAKE SELECTION ENTRY...
  CONCATENATE WA_PREV_YEAR WA_PREV_MONTH '01' INTO WA_S_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_S_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_E_DAY.

  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, ZTPM_OPTIME, IT_BDTIME[].

*** Get No of breakdown
    PERFORM GET_NO_BREAKDOWN USING IT_SHOP-SHOP
                                   WA_S_DAY
                                   WA_E_DAY.

    SELECT SINGLE  *
           FROM  ZTPM_OPTIME
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = WA_PREV_YEAR
           AND   ZMONTH = WA_PREV_MONTH.
    IF WA_BDNUM  NE 0.
      WA_VALUE =  ZTPM_OPTIME-OPTIME / WA_BDNUM.
    ENDIF.

    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                              WA_VALUE.

  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.


**** Target
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_AVRATE, ZTPM_MONBD, ZTPM_OPTIME.

    SELECT SINGLE  AVRATE  INTO  WA_AVRATE
           FROM  ZTPM_MTBT
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH
           AND   ZMTBT  = 'MTBF'.

    MOVE : WA_AVRATE  TO WA_VALUE.
    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

**** Actual
*** MAKE SELECTION ENTRY...
  CONCATENATE P_YEAR P_MONTH '01' INTO WA_S_DAY.
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_S_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_E_DAY.

  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, IT_BDTIME, IT_BDTIME[].
*** Get No of breakdown
    PERFORM GET_NO_BREAKDOWN USING IT_SHOP-SHOP
                                   WA_S_DAY
                                   WA_E_DAY.

    SELECT SINGLE  *
           FROM  ZTPM_OPTIME
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH.
    IF WA_BDNUM  NE 0.
      WA_VALUE =  ZTPM_OPTIME-OPTIME / WA_BDNUM.
    ENDIF.

    PERFORM SET_IT_ACTUAL USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.
ENDFORM.                    " RE_CALC_MTBF
*&---------------------------------------------------------------------*
*&      Form  READ_PREVENTIVE_MAINTENANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PREVENTIVE_MAINTENANCE.
**** No of Daily inspection + No of Planned Maintenance
**** Prev.Month Actual..............................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_ZACTAL.
    SELECT SUM( ZACTAL )
           INTO  WA_ZACTAL
           FROM   ZTPM_PMRO
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = WA_PREV_YEAR
           AND    ZMONTH = WA_PREV_MONTH
           AND    AUART  IN ('PM04', 'PM01').

    MOVE : WA_ZACTAL TO WA_VALUE.

    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                              WA_VALUE.

  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.

**** Target..............................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_ZPLAND.
    SELECT SUM( ZPLAND )
           INTO  WA_ZPLAND
           FROM   ZTPM_PMRO
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = P_YEAR
           AND    ZMONTH = P_MONTH
           AND    AUART  IN ('PM04', 'PM01').

    MOVE : WA_ZPLAND TO WA_VALUE.

    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

**** Actual............................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_ZACTAL.
    SELECT SUM( ZACTAL )
           INTO  WA_ZACTAL
           FROM   ZTPM_PMRO
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = P_YEAR
           AND    ZMONTH = P_MONTH
           AND    AUART  IN ('PM04', 'PM01').

    MOVE : WA_ZACTAL TO WA_VALUE.

    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.

ENDFORM.                    " READ_PREVENTIVE_MAINTENANCE
*&---------------------------------------------------------------------*
*&      Form  RE_CALC_PREVENTIVE_MAINTENANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_CALC_PREVENTIVE_MAINTENANCE.
  CLEAR : IT_AUART, IT_AUART[].

  MOVE : 'PM04' TO IT_AUART-AUART.
  APPEND IT_AUART.
  MOVE : 'PM01' TO IT_AUART-AUART.
  APPEND IT_AUART.
  MOVE : 'PM03' TO IT_AUART-AUART.
  APPEND IT_AUART.
  MOVE : 'PM05' TO IT_AUART-AUART.
  APPEND IT_AUART.

**** Prev.Month Actual....................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, IT_PMROX, IT_PMROX[].

    CALL FUNCTION 'Z_FPM_MAINTENANCE_RATIO'
         EXPORTING
              I_SHOP   = IT_SHOP-SHOP
              I_AJAHR  = WA_PREV_YEAR
              I_ZMONTH = WA_PREV_MONTH
         TABLES
              T_PMRO   = IT_PMROX
              T_AUART  = IT_AUART.

    READ TABLE IT_PMROX WITH KEY SHOP   = IT_SHOP-SHOP
                                 AJAHR  = WA_PREV_YEAR
                                 ZMONTH = WA_PREV_MONTH
                                 AUART  = 'PM04'.

    MOVE IT_PMROX-ZACTAL TO WA_VALUE.

    READ TABLE IT_PMROX WITH KEY SHOP   = IT_SHOP-SHOP
                                 AJAHR  = WA_PREV_YEAR
                                 ZMONTH = WA_PREV_MONTH
                                 AUART  = 'PM01'.

    WA_VALUE = WA_VALUE +  IT_PMROX-ZACTAL.

    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                              WA_VALUE.

  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.

**** Target......................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, IT_PMROX, IT_PMROX[].

    CALL FUNCTION 'Z_FPM_MAINTENANCE_RATIO'
         EXPORTING
              I_SHOP   = IT_SHOP-SHOP
              I_AJAHR  = P_YEAR
              I_ZMONTH = P_MONTH
         TABLES
              T_PMRO   = IT_PMROX
              T_AUART  = IT_AUART.

    READ TABLE IT_PMROX WITH KEY SHOP   = IT_SHOP-SHOP
                                 AJAHR  = P_YEAR
                                 ZMONTH = P_MONTH
                                 AUART  = 'PM04'.

    MOVE IT_PMROX-ZPLAND TO WA_VALUE.

    READ TABLE IT_PMROX WITH KEY SHOP   = IT_SHOP-SHOP
                                 AJAHR  = P_YEAR
                                 ZMONTH = P_MONTH
                                 AUART  = 'PM01'.

    WA_VALUE = WA_VALUE +  IT_PMROX-ZPLAND.

    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

**** Actual..............................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, IT_PMROX, IT_PMROX[].

    CALL FUNCTION 'Z_FPM_MAINTENANCE_RATIO'
         EXPORTING
              I_SHOP   = IT_SHOP-SHOP
              I_AJAHR  = P_YEAR
              I_ZMONTH = P_MONTH
         TABLES
              T_PMRO   = IT_PMROX
              T_AUART  = IT_AUART.

    READ TABLE IT_PMROX WITH KEY SHOP   = IT_SHOP-SHOP
                                 AJAHR  = P_YEAR
                                 ZMONTH = P_MONTH
                                 AUART  = 'PM04'.

    MOVE IT_PMROX-ZACTAL TO WA_VALUE.

    READ TABLE IT_PMROX WITH KEY SHOP   = IT_SHOP-SHOP
                                 AJAHR  = P_YEAR
                                 ZMONTH = P_MONTH
                                 AUART  = 'PM01'.

    WA_VALUE = WA_VALUE +  IT_PMROX-ZACTAL.

    PERFORM SET_IT_ACTUAL USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.

ENDFORM.                    " RE_CALC_PREVENTIVE_MAINTENANCE
*&---------------------------------------------------------------------*
*&      Form  READ_BREAKDOWN_MAINTENANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_BREAKDOWN_MAINTENANCE.
**** Prev.Month Actual..............................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_ZACTAL.
    SELECT SUM( ZACTAL )
           INTO  WA_ZACTAL
           FROM   ZTPM_PMRO
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = WA_PREV_YEAR
           AND    ZMONTH = WA_PREV_MONTH
           AND    AUART  = 'PM02'.

    MOVE : WA_ZACTAL TO WA_VALUE.

    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                              WA_VALUE.

  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.

**** Target.................................................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_ZPLAND.
    SELECT SUM( ZPLAND )
           INTO  WA_ZPLAND
           FROM   ZTPM_PMRO
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = P_YEAR
           AND    ZMONTH = P_MONTH
           AND    AUART  = 'PM02'.

    MOVE : WA_ZPLAND TO WA_VALUE.

    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

**** Actual...........................................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_ZACTAL.
    SELECT SUM( ZACTAL )
           INTO  WA_ZACTAL
           FROM   ZTPM_PMRO
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = P_YEAR
           AND    ZMONTH = P_MONTH
           AND    AUART  = 'PM02'.

    MOVE : WA_ZACTAL TO WA_VALUE.

    PERFORM SET_IT_ACTUAL USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.

ENDFORM.                    " READ_BREAKDOWN_MAINTENANCE
*&---------------------------------------------------------------------*
*&      Form  RE_CALC_BREAKDOWN_MAINTENANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_CALC_BREAKDOWN_MAINTENANCE.
  CLEAR : IT_AUART, IT_AUART[].

  MOVE : 'PM02' TO IT_AUART-AUART.
  APPEND IT_AUART.

**** Prev.Month Actual..................................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, IT_PMROX, IT_PMROX[].

    CALL FUNCTION 'Z_FPM_MAINTENANCE_RATIO'
         EXPORTING
              I_SHOP   = IT_SHOP-SHOP
              I_AJAHR  = WA_PREV_YEAR
              I_ZMONTH = WA_PREV_MONTH
         TABLES
              T_PMRO   = IT_PMROX
              T_AUART  = IT_AUART.

    READ TABLE IT_PMROX WITH KEY SHOP   = IT_SHOP-SHOP
                                 AJAHR  = WA_PREV_YEAR
                                 ZMONTH = WA_PREV_MONTH
                                 AUART  = 'PM02'.

    MOVE IT_PMROX-ZACTAL TO WA_VALUE.

    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                              WA_VALUE.

  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.

**** Target................................................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, IT_PMROX, IT_PMROX[].

    CALL FUNCTION 'Z_FPM_MAINTENANCE_RATIO'
         EXPORTING
              I_SHOP   = IT_SHOP-SHOP
              I_AJAHR  = P_YEAR
              I_ZMONTH = P_MONTH
         TABLES
              T_PMRO   = IT_PMROX
              T_AUART  = IT_AUART.

    READ TABLE IT_PMROX WITH KEY SHOP   = IT_SHOP-SHOP
                                 AJAHR  = P_YEAR
                                 ZMONTH = P_MONTH
                                 AUART  = 'PM02'.

    MOVE IT_PMROX-ZPLAND TO WA_VALUE.

    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

**** Actual........................................................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, IT_PMROX, IT_PMROX[].

    CALL FUNCTION 'Z_FPM_MAINTENANCE_RATIO'
         EXPORTING
              I_SHOP   = IT_SHOP-SHOP
              I_AJAHR  = P_YEAR
              I_ZMONTH = P_MONTH
         TABLES
              T_PMRO   = IT_PMROX
              T_AUART  = IT_AUART.

    READ TABLE IT_PMROX WITH KEY SHOP   = IT_SHOP-SHOP
                                 AJAHR  = P_YEAR
                                 ZMONTH = P_MONTH
                                 AUART  = 'PM02'.

    MOVE IT_PMROX-ZACTAL TO WA_VALUE.

    PERFORM SET_IT_ACTUAL USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.
ENDFORM.                    " RE_CALC_BREAKDOWN_MAINTENANCE
*&---------------------------------------------------------------------*
*&      Form  READ_MAINTENANCE_COST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MAINTENANCE_COST.
**** Prev.Month Actual....................................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_ZACOST, ZTPM_PMCO.
    SELECT SINGLE  ZACOST  INTO  WA_ZACOST
           FROM   ZTPM_PMCO
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = WA_PREV_YEAR
           AND    ZMONTH = WA_PREV_MONTH.

    MOVE : WA_ZACOST TO WA_VALUE.

    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                              WA_VALUE.

  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.

**** Target............................................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_ZPCOST, ZTPM_PLANDCO.
    SELECT SINGLE  ZPCOST  INTO  WA_ZPCOST
           FROM  ZTPM_PLANDCO
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH.

    MOVE : WA_ZPCOST TO WA_VALUE.

    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

**** Actual..............................................
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_ZACOST, ZTPM_PMCO.
    SELECT SINGLE  ZACOST  INTO  WA_ZACOST
           FROM   ZTPM_PMCO
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = P_YEAR
           AND    ZMONTH = P_MONTH.

    MOVE : WA_ZACOST TO WA_VALUE.

    PERFORM SET_IT_ACTUAL USING IT_SHOP-SHOP
                              WA_VALUE.


  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.

ENDFORM.                    " READ_MAINTENANCE_COST
*&---------------------------------------------------------------------*
*&      Form  RE_CALC_MAINTENANCE_COST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_CALC_MAINTENANCE_COST.
  DATA : BEGIN OF IT_OBJECT OCCURS 0,
           SHKZG LIKE AUFM-SHKZG,
           DMBTR LIKE AUFM-DMBTR,
         END OF IT_OBJECT.

  DATA: WA_S_DAY LIKE SY-DATUM,
        WA_E_DAY LIKE SY-DATUM.

  CONCATENATE WA_PREV_YEAR WA_PREV_MONTH '01' INTO WA_S_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_S_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_E_DAY.

**** Prev.Month Actual.
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE.
    SELECT  A~SHKZG A~DMBTR
            INTO CORRESPONDING FIELDS OF TABLE IT_OBJECT
            FROM AUFM AS A
                 INNER JOIN VIAUFKST AS B
                 ON  A~AUFNR = B~AUFNR
            WHERE A~BUDAT BETWEEN WA_S_DAY AND WA_E_DAY
            AND   B~AUART IN ('PM01', 'PM02', 'PM03', 'PM04')
            AND   B~INGPR = IT_SHOP-SHOP.
    LOOP AT IT_OBJECT.
      IF IT_OBJECT-SHKZG  = 'S'.
        IT_OBJECT-DMBTR = IT_OBJECT-DMBTR * ( -1 ).
      ENDIF.

      WA_VALUE = WA_VALUE + IT_OBJECT-DMBTR.
    ENDLOOP.

    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                                WA_VALUE.
  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.

**** Target
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_ZPCOST, ZTPM_PLANDCO.
    SELECT SINGLE  ZPCOST  INTO  WA_ZPCOST
           FROM  ZTPM_PLANDCO
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH.

    MOVE : WA_ZPCOST TO WA_VALUE.

    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

  CONCATENATE P_YEAR P_MONTH '01' INTO WA_S_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_S_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_E_DAY.

**** Actual
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE.
    SELECT  A~SHKZG A~DMBTR
            INTO CORRESPONDING FIELDS OF TABLE IT_OBJECT
            FROM AUFM AS A
                 INNER JOIN VIAUFKST AS B
                 ON  A~AUFNR = B~AUFNR
            WHERE A~BUDAT BETWEEN WA_S_DAY AND WA_E_DAY
            AND   B~AUART IN ('PM01', 'PM02', 'PM03', 'PM04')
            AND   B~INGPR = IT_SHOP-SHOP.
    LOOP AT IT_OBJECT.
      IF IT_OBJECT-SHKZG  = 'S'.
        IT_OBJECT-DMBTR = IT_OBJECT-DMBTR * ( -1 ).
      ENDIF.

      WA_VALUE = WA_VALUE + IT_OBJECT-DMBTR.
    ENDLOOP.

    PERFORM SET_IT_ACTUAL USING IT_SHOP-SHOP
                              WA_VALUE.


  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.
ENDFORM.                    " RE_CALC_MAINTENANCE_COST
*&---------------------------------------------------------------------*
*&      Form  READ_COST_PER_UNIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_COST_PER_UNIT.
**** Prev.Month Actual.
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, ZTPM_PMCO.
    SELECT SINGLE  ZAUNITC  INTO  WA_VALUE
           FROM   ZTPM_PMCO
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = WA_PREV_YEAR
           AND    ZMONTH = WA_PREV_MONTH.

    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                              WA_VALUE.

  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.

**** Target
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_ZPUNITC, ZTPM_PLANDCO.
    SELECT SINGLE  ZPUNITC  INTO  WA_ZPUNITC
           FROM  ZTPM_PLANDCO
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH.

    MOVE : WA_ZPUNITC TO WA_VALUE.

    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

**** Actual
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_ZAUNITC, ZTPM_PMCO.
    SELECT SINGLE  ZAUNITC  INTO  WA_ZAUNITC
           FROM   ZTPM_PMCO
           WHERE  SHOP   = IT_SHOP-SHOP
           AND    AJAHR  = P_YEAR
           AND    ZMONTH = P_MONTH.

    MOVE : WA_ZAUNITC TO WA_VALUE.

    PERFORM SET_IT_ACTUAL USING IT_SHOP-SHOP
                              WA_VALUE.


  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.

ENDFORM.                    " READ_COST_PER_UNIT
*&---------------------------------------------------------------------*
*&      Form  RE_CALC_COST_PER_UNIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_CALC_COST_PER_UNIT.
  DATA : BEGIN OF IT_OBJECT OCCURS 0,
           SHKZG LIKE AUFM-SHKZG,
           DMBTR LIKE AUFM-DMBTR,
         END OF IT_OBJECT.

  DATA : WA_S_DAY LIKE SY-DATUM,
         WA_E_DAY LIKE SY-DATUM.

  DATA : WA_CURR_AMOUNT LIKE MLCD-LBKUM,
         WA_SUM_ACTUAL  TYPE WTGXXX.

  CONCATENATE WA_PREV_YEAR WA_PREV_MONTH '01' INTO WA_S_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_S_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_E_DAY.

**** Prev.Month Actual.
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_SUM_ACTUAL, WA_CURR_AMOUNT.
    SELECT  A~SHKZG A~DMBTR
            INTO CORRESPONDING FIELDS OF TABLE IT_OBJECT
            FROM AUFM AS A
                 INNER JOIN VIAUFKST AS B
                 ON  A~AUFNR = B~AUFNR
            WHERE A~BUDAT BETWEEN WA_S_DAY AND WA_E_DAY
            AND   B~AUART IN ('PM01', 'PM02', 'PM03', 'PM04')
            AND   B~INGPR = IT_SHOP-SHOP.
    LOOP AT IT_OBJECT.
      IF IT_OBJECT-SHKZG  = 'S'.
        IT_OBJECT-DMBTR = IT_OBJECT-DMBTR * ( -1 ).
      ENDIF.

      WA_SUM_ACTUAL = WA_SUM_ACTUAL + IT_OBJECT-DMBTR.
    ENDLOOP.

    SELECT SINGLE ZUNIT INTO WA_CURR_AMOUNT
           FROM  ZTPM_PUNIT
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH.

    IF NOT WA_CURR_AMOUNT IS INITIAL.
      WA_VALUE = WA_SUM_ACTUAL  / WA_CURR_AMOUNT.
    ELSE.
      MOVE : ' ' TO WA_VALUE.
    ENDIF.

    PERFORM SET_IT_PREV USING IT_SHOP-SHOP
                                WA_VALUE.
  ENDLOOP.
  APPEND IT_PREV.   CLEAR : IT_PREV.

**** Target
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_ZPUNITC, ZTPM_PLANDCO.
    SELECT SINGLE  ZPUNITC  INTO  WA_ZPUNITC
           FROM  ZTPM_PLANDCO
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH.

    MOVE : WA_ZPUNITC TO WA_VALUE.
    PERFORM SET_IT_TARGET USING IT_SHOP-SHOP
                                WA_VALUE.

  ENDLOOP.
  APPEND IT_TARGET.   CLEAR : IT_TARGET.

  CONCATENATE P_YEAR P_MONTH '01' INTO WA_S_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_S_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_E_DAY.

**** Actual
  LOOP AT IT_SHOP.
    CLEAR : WA_VALUE, WA_SUM_ACTUAL, WA_CURR_AMOUNT.
    SELECT  A~SHKZG A~DMBTR
            INTO CORRESPONDING FIELDS OF TABLE IT_OBJECT
            FROM AUFM AS A
                 INNER JOIN VIAUFKST AS B
                 ON  A~AUFNR = B~AUFNR
            WHERE A~BUDAT BETWEEN WA_S_DAY AND WA_E_DAY
            AND   B~AUART IN ('PM01', 'PM02', 'PM03', 'PM04')
            AND   B~INGPR = IT_SHOP-SHOP.
    LOOP AT IT_OBJECT.
      IF IT_OBJECT-SHKZG  = 'S'.
        IT_OBJECT-DMBTR = IT_OBJECT-DMBTR * ( -1 ).
      ENDIF.

      WA_SUM_ACTUAL = WA_SUM_ACTUAL + IT_OBJECT-DMBTR.
    ENDLOOP.

    SELECT SINGLE ZUNIT INTO WA_CURR_AMOUNT
           FROM  ZTPM_PUNIT
           WHERE SHOP   = IT_SHOP-SHOP
           AND   AJAHR  = P_YEAR
           AND   ZMONTH = P_MONTH.

    IF NOT WA_CURR_AMOUNT IS INITIAL.
      WA_VALUE = WA_SUM_ACTUAL  / WA_CURR_AMOUNT.
    ELSE.
      MOVE : ' ' TO WA_VALUE.
    ENDIF.

    PERFORM SET_IT_ACTUAL USING IT_SHOP-SHOP
                              WA_VALUE.


  ENDLOOP.
  APPEND IT_ACTUAL.   CLEAR : IT_ACTUAL.
ENDFORM.                    " RE_CALC_COST_PER_UNIT
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
