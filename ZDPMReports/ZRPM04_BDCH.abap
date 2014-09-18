************************************************************************
* Program Name      : ZRPM04_BDCH
* Author            : Myoungho Park
* Creation Date     : 2003.08.04.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :  This Program is Shop Breakdown Trend Report
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

REPORT  ZRPM04_BDCH   MESSAGE-ID DEMOOFFICEINTEGRATIO.


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

CONSTANTS: C_LINE_COUNT TYPE I VALUE 5.
**********************************************************************
**********************************************************************

*---Global Variables & Tables
FIELD-SYMBOLS: <MONTH>,
               <YEAR> .

TABLES: ZSPM_PARAM,  "//Parameters
*        ZTPM_SHOP,   "//
        ZTPM_ANBD,   "//Annual average breakdown rate
        ZTPM_OPTIME, "//Operation Time
        ZTPM_MONBD,  "//Monthly Breakdown Rate (Planed)
        T024I.       "//Maintenance planner groups
*        T357.        "//Plant Section

*** Base Shop list
DATA: BEGIN OF IT_SHOP OCCURS 0,
        SHOP LIKE ZSPM_PARAM-SHOP,
      END OF IT_SHOP.

*** Base Year list
DATA: BEGIN OF IT_YEAR OCCURS 0,
         AJAHR LIKE ZTPM_OPTIME-AJAHR,
      END OF IT_YEAR.

*** Base Year/Month List...
DATA: BEGIN OF IT_RANGE OCCURS 0,
*         AJAHR  LIKE ZTPM_MONBD-AJAHR,
         ZMONTH LIKE ZSPM_PARAM-ZMONTH,
      END OF IT_RANGE.

*** Annually average breakdown rate
DATA: IT_ZTPM_ANBD LIKE ZTPM_ANBD OCCURS 0 WITH HEADER LINE.

*** Monthly average breakdown rate
DATA: IT_RATE LIKE ZSPM_BDMON OCCURS 0 WITH HEADER LINE.

*** For LinkServer "Monthly"
DATA: IT_TEMP_MONBD LIKE ZSPM_MONTH2 OCCURS 0.
DATA: IT_MONBD LIKE ZSPM_MONTH2 OCCURS 0 WITH HEADER LINE.

*** For LinkServer 'Annually'
DATA: IT_TEMP_ANBD LIKE ZSPM_YEAR OCCURS 0.
DATA: IT_ANBD LIKE ZSPM_YEAR OCCURS 0 WITH HEADER LINE.


DATA: IT_TEMP LIKE ZTPM_MONBD OCCURS 0 WITH HEADER LINE.

DATA: IT_TEMP_RATE LIKE ZSPM_BDMON OCCURS 0 WITH HEADER LINE.

DATA : P_ZMONTH LIKE ZSPM_PARAM-ZMONTH.
DATA : WA_MONTH LIKE ZSPM_PARAM-MONTH.


*** Other Global variables
DATA: WA_S_YEAR LIKE ZTPM_ANBD-AJAHR,
      WA_E_YEAR LIKE ZTPM_ANBD-AJAHR,
      WA_SHTXT LIKE ZTPM_SHOP-SHTXT,
      WA_INIT.

DATA : WA_YEAR_SUM    LIKE ZSPM_BDMON-AVRATE,
       WA_MONTH_COUNT TYPE I,
       WA_AVRATE      LIKE ZSPM_BDMON-AVRATE,
       WA_SUM_AVRATE      LIKE ZSPM_BDMON-AVRATE.

DATA : WA_CURR_YEAR_SUM LIKE ZSPM_BDMON-AVRATE,
       WA_CURR_MONTH_COUNT TYPE I,
       WA_CURR_AVRATE      LIKE ZSPM_BDMON-AVRATE.

DATA : BEGIN OF WA_DYNPFIELDS OCCURS 100.
        INCLUDE STRUCTURE DYNPREAD.
DATA : END OF WA_DYNPFIELDS.

DATA : WA_OPTIME LIKE ZTPM_OPTIME-OPTIME.

DATA: WA_DYNAME LIKE D020S-PROG.
DATA: WA_DYNUMB LIKE D020S-DNUM.
*---------------------------------------------------------------------*
*       CLASS c_event_handler DEFINITION
*---------------------------------------------------------------------*
*      Copy & Modify from Standard Program SAPRDEMOEXCELINTEGRATION2
*
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
*        Copy & Modify from Standard Program SAPRDEMOEXCELINTEGRATION2
*
*---------------------------------------------------------------------*
CLASS C_EVENT_HANDLER IMPLEMENTATION.
  METHOD CLOSE_EVENT_HANDLER.
    DATA: ANSWER.
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

SELECTION-SCREEN COMMENT (5) TEXT-004.
PARAMETER : P_MONTH LIKE ZSPM_PARAM-MONTH DEFAULT SY-DATUM+4(2)
                                          OBLIGATORY.

SELECTION-SCREEN POSITION 30.
SELECTION-SCREEN COMMENT 26(5) TEXT-002.
PARAMETER : P_YEAR LIKE ZSPM_PARAM-AJAHR DEFAULT SY-DATUM(4)
                                         OBLIGATORY.

SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS : S_SHOP  FOR ZSPM_PARAM-SHOP  NO INTERVALS
                                              NO-EXTENSION.

*PARAMETER : P_PLACE LIKE ZSPM_PARAM-INPLACE DEFAULT 'X'
*                                            AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK BLOCK1.

******************* INITIALIZATION ********************************
*******************************************************************
INITIALIZATION.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_ZMONTH .
*  PERFORM SELECT_MONTH.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_YEAR.
  PERFORM SELECT_YEAR.
***************** AT SELECTION-SCREEN ******************************
********************************************************************
AT SELECTION-SCREEN.

  CASE SY-UCOMM.
    WHEN 'ONLI'.
      CLEAR: SY-UCOMM.
      CONCATENATE P_YEAR P_MONTH INTO P_ZMONTH.
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
*       Copy & Modify from Standard Program SAPRDEMOEXCELINTEGRATION2
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
*       Object container create
*       GET_LINK_SERVER
*       START_LINK_SERVER
*       GET_DOCUMENT_PROXY
*       REFRESH_LINKS
*       PERFORM OPEN_DOCUMENT
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

**** Create Container Control 'CONTAINER_01'
    CREATE OBJECT CONTAINER
              EXPORTING CONTAINER_NAME = 'CONTAINER_01'.

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
****  Link Server name SUFFIX : 'BDCH'
    CALL METHOD LINK_SERVER->START_LINK_SERVER
                      EXPORTING LINK_SERVER_MODE =
                                LINK_SERVER->LINK_SERVER_CUSTOMNAME
                                SERVER_NAME_SUFFIX = 'BDCH'
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
*       Link LinkServer to Internal Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_LINKS.

*  PERFORM READ_DATA.

  PERFORM READ_DATA2.

  CLEAR: IT_TEMP_MONBD[], IT_TEMP_ANBD[].

  DATA: WA_FIELD_NAME(30),
        WA_YEAR(4),
        WA_MONTH(10), "TYPE D,
        WA_COUNT(2) TYPE N.

  DATA: ERROR TYPE REF TO I_OI_ERROR.

  IF NOT LINK_SERVER IS INITIAL.
****** monthly data
    IT_TEMP_MONBD[] = IT_MONBD[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Monthly'
                       ITEM_TITLE  = 'Monthly Brekdown Rate'
                       DDIC_NAME   = 'ZSPM_MONTH2'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_MONBD.
******** annual data
    IT_TEMP_ANBD[] = IT_ANBD[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Annually'
                       ITEM_TITLE  = 'Annaully Reakdown Rate'
                       DDIC_NAME   = 'ZSPM_YEAR'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_ANBD.
******* month
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
**** Report date
    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
                EXPORTING ITEM_NAME = 'report_date'
                          ITEM_VALUE  = SY-DATUM
                          NO_FLUSH    = 'X'
                IMPORTING RETCODE = RETCODE.
******* year
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

  CONCATENATE P_YEAR P_MONTH INTO P_ZMONTH.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR: SY-UCOMM.
      PERFORM FREE_OBJECT.
*      LEAVE PROGRAM.
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
*        Copy & Modify from Standard Program SAPRDEMOEXCELINTEGRATION2
*----------------------------------------------------------------------*
*      <--P_DOCUMENT_TYPE  text
*      <--P_DOCUMENT_FORMAT  text
*      <--P_DOC_URL  text
*----------------------------------------------------------------------*
FORM LOAD_DOCUMENT CHANGING DOCUMENT_TYPE   TYPE C
                            DOCUMENT_FORMAT TYPE C
                            DOC_URL         TYPE T_URL.


*** Documents are managed BDS (Business Document Service)
*** T-code : OAOR

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
        DOC_OBJECT_KEY TYPE SBDST_OBJECT_KEY VALUE 'TREND',
        DOC_MIMETYPE LIKE BAPICOMPON-MIMETYPE.


  CLEAR DOC_URL.

  WA_DOC_SIGNATURE-PROP_NAME = 'DESCRIPTION'.
  WA_DOC_SIGNATURE-PROP_VALUE = 'Shop Breakdown Trend'.
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
****** get document URL on sap server...
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

*  LEAVE PROGRAM.
ENDFORM.                    " FREE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       Read data each Shop's Breakdown Rate...
*       By annually and monthly
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
*  DATA: WA_YEAR(2)   TYPE N,      "LIKE ZTPM_ANBD-AJAHR,
*        WA_DATE      LIKE SY-DATUM,
*        CAL_DATE     LIKE SY-DATUM,
*        WA_MONTH(2)  TYPE N,
*        WA_COUNT     LIKE T5A4A-DLYMO.
*
*  CLEAR: IT_SHOP, IT_SHOP[], IT_YEAR, IT_YEAR[],
*         IT_ZTPM_ANBD, IT_ZTPM_ANBD[],
*         IT_ANBD, IT_ANBD[], IT_RANGE, IT_RANGE[],
*         IT_MONBD, IT_MONBD[].
*
*  CLEAR: WA_S_YEAR, WA_E_YEAR.
*
*  CLEAR: WA_CURR_AVRATE, WA_CURR_YEAR_SUM, WA_CURR_MONTH_COUNT.
*
*  IF SY-DYNNR EQ '0101' AND S_SHOP-LOW NE SPACE.
*    S_SHOP-SIGN = 'I'.
*    S_SHOP-OPTION = 'EQ'.
*    APPEND S_SHOP.
*  ENDIF.
*
**** select Shop data from T357(Plant Section)
**  SELECT DISTINCT BEBER AS SHOP
**          INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
**          FROM  T357
**          WHERE BEBER IN S_SHOP.
*
*  SELECT  DISTINCT INGRP AS SHOP
*          INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
*          FROM  T024I
*          WHERE INGRP IN S_SHOP.
*  IF SY-SUBRC NE 0.
*    MESSAGE E000(ZMPM) WITH TEXT-002.
*    LEAVE TO SCREEN 0.
*  ENDIF.
*
**** Make up Year range...
*  IT_YEAR-AJAHR = P_ZMONTH(4).
*  WA_E_YEAR = IT_YEAR-AJAHR.
*  APPEND IT_YEAR.
*  IT_YEAR-AJAHR = P_ZMONTH(4) - 1.
*  APPEND IT_YEAR.
*  IT_YEAR-AJAHR = P_ZMONTH(4) - 2.
*  APPEND IT_YEAR.
*  IT_YEAR-AJAHR = P_ZMONTH(4) - 3.
*  APPEND IT_YEAR.
*  WA_S_YEAR = IT_YEAR-AJAHR.
*  SORT  IT_YEAR BY AJAHR. "DESCENDING.
*
*
**** get Annually Breakdown rate...
**  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZTPM_ANBD
**           FROM  ZTPM_ANBD
**           WHERE SHOP  IN S_SHOP
**           AND   AJAHR BETWEEN WA_S_YEAR AND WA_E_YEAR.
*
**** Rearrange Internal Table to Plat Structure
*****  like month_01, month_02, month_03.....
*  LOOP AT IT_SHOP.
*    LOOP AT IT_YEAR.
*      CLEAR: WA_AVRATE, WA_YEAR_SUM, WA_MONTH_COUNT.
*      WA_YEAR = SY-TABIX.
*
*      SELECT   SUM( AVRATE ) COUNT( * )
*               INTO  (WA_YEAR_SUM, WA_MONTH_COUNT)
*               FROM  ZTPM_MONBD
*               WHERE SHOP  = IT_SHOP-SHOP
*               AND   AJAHR = IT_YEAR-AJAHR.
*      IF SY-SUBRC EQ 0.
*        IF NOT WA_MONTH_COUNT IS INITIAL.
*          WA_AVRATE = WA_YEAR_SUM / WA_MONTH_COUNT.
*        ENDIF.
*
*        PERFORM SET_IT_ANBD USING WA_YEAR
*                                  WA_AVRATE .
*      ENDIF.
*    ENDLOOP.
*
*    MOVE: IT_SHOP-SHOP TO IT_ANBD-SHOP.
*    SELECT SINGLE INNAM  INTO IT_ANBD-SHTXT
*                  FROM T024I
*                  WHERE INGRP = IT_SHOP-SHOP.
*    APPEND IT_ANBD.
*    CLEAR: WA_YEAR, IT_ANBD.
*  ENDLOOP.
*
*  SORT IT_ANBD BY SHOP.
*
**** make first day for select condition
*  CONCATENATE  P_ZMONTH  '01' INTO WA_DATE.
*
**** make select entry (year/month)
*  DO 12 TIMES.
**    MOVE SY-INDEX TO WA_COUNT.
*    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*         EXPORTING
*              DATE      = WA_DATE
*              DAYS      = 0
*              MONTHS    = WA_COUNT
*              SIGNUM    = '-'
*              YEARS     = 0
*         IMPORTING
*              CALC_DATE = CAL_DATE.
*    MOVE : CAL_DATE(6) TO IT_RANGE-ZMONTH.
*    APPEND IT_RANGE.
*    MOVE SY-INDEX TO WA_COUNT.
*  ENDDO.
*
*  SORT IT_RANGE BY ZMONTH ASCENDING.
*
*  LOOP AT IT_SHOP.
*    LOOP AT IT_RANGE.
*      CLEAR: IT_RATE, IT_RATE[].
*
*      WA_MONTH = SY-TABIX.
**** Calculate Actual Monthly Breakdown rate...
**** Only Current month and 1month prev
*      IF WA_MONTH = 11 OR WA_MONTH = 12.
*        CALL FUNCTION 'Z_FPM_CAL_BREAKDOWN_RATE_MON'
*          EXPORTING
*            I_MONTH       = IT_RANGE-ZMONTH
**           I_PLANT       =
*            I_SHOP        = IT_SHOP-SHOP
*            I_MAUEH       = 'MIN'
*          TABLES
*            T_RATE        = IT_RATE.
*
*        READ TABLE IT_RATE INDEX 1.
*      ELSE.
*****  Orthers read form table ZTPM_MONBD....
*        SELECT SINGLE AVRATE INTO IT_RATE-AVRATE
*                       FROM ZTPM_MONBD
*                       WHERE SHOP   = IT_SHOP-SHOP
*                       AND   AJAHR  = IT_RANGE-ZMONTH(4)
*                       AND   ZMONTH = IT_RANGE-ZMONTH.
*
*      ENDIF.
*      PERFORM SET_IT_MONBD USING WA_MONTH
*                                 IT_RATE-AVRATE.
***** for current years average ...
*      IF IT_RANGE-ZMONTH(4) = SY-DATUM(4).
*        WA_CURR_YEAR_SUM    = WA_CURR_YEAR_SUM + IT_RATE-AVRATE.
*        WA_CURR_MONTH_COUNT = WA_CURR_MONTH_COUNT + 1.
*      ENDIF.
*    ENDLOOP.
*
*    WA_CURR_AVRATE = WA_CURR_YEAR_SUM / WA_CURR_MONTH_COUNT.
*    READ TABLE IT_ANBD WITH KEY SHOP = IT_SHOP-SHOP.
*    MOVE WA_CURR_AVRATE TO IT_ANBD-YEAR04.
*    MODIFY IT_ANBD TRANSPORTING YEAR04 WHERE SHOP = IT_SHOP-SHOP.
*
*    CLEAR: WA_CURR_AVRATE, WA_CURR_YEAR_SUM, WA_CURR_MONTH_COUNT.
***** for current years average ...
*
**    MOVE: IT_SHOP-SHOP TO IT_MONBD-SHOP.
*    APPEND IT_MONBD.
*    CLEAR: WA_MONTH, IT_MONBD.
*  ENDLOOP.
*
**** Append space fields to internal table for filling empty cells
**** Excel display empty cell "N/A"
*  PERFORM FILL_EMPTY_CELL.
*
**  SORT IT_MONBD BY SHOP.
ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  OPEN_DOCUMENT
*&---------------------------------------------------------------------*
*        Copy & Modify from Standard Program SAPRDEMOEXCELINTEGRATION2
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
                                       OPEN_INPLACE =  'X'  "//P_PLACE
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

*  P_AVRATE = P_AVRATE * 100.

  CONCATENATE 'IT_MONBD-MONTH' P_ZMONTH INTO WA_MONTH.
  ASSIGN  (WA_MONTH) TO <MONTH>.
  MOVE  : P_AVRATE   TO <MONTH>.

ENDFORM.                    " SET_IT_MONBD
*&---------------------------------------------------------------------*
*&      Form  SET_IT_ANBD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_YEAR  text
*      -->P_IT_ZTPM_ANBD_AVRATE  text
*----------------------------------------------------------------------*
FORM SET_IT_ANBD USING    P_YEAR
                          P_AVRATE.
  DATA: WA_YEAR LIKE FELD-NAME.

*  P_AVRATE = P_AVRATE * 100.

  CONCATENATE 'IT_ANBD-YEAR' P_YEAR INTO WA_YEAR.
  ASSIGN  (WA_YEAR) TO <YEAR>.
  MOVE  : P_AVRATE  TO <YEAR>.
ENDFORM.                    " SET_IT_ANBD
*&---------------------------------------------------------------------*
*&      Form  SELECT_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_MONTH.
  DATA SPMON LIKE ZSPM_PARAM-ZMONTH.

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

    WA_DYNAME = SY-REPID.
    WA_DYNUMB = SY-DYNNR.

    WA_DYNPFIELDS-FIELDNAME  = 'P_ZMONTH'.
    WA_DYNPFIELDS-FIELDVALUE = SPMON.
    APPEND WA_DYNPFIELDS.



    CALL FUNCTION 'DYNP_VALUES_UPDATE'
         EXPORTING
              DYNAME     = WA_DYNAME
              DYNUMB     = WA_DYNUMB
         TABLES
              DYNPFIELDS = WA_DYNPFIELDS.

*    P_ZMONTH = SPMON.
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
    DESCRIBE TABLE IT_ANBD LINES WA_LINES.
    IF WA_LINES < C_LINE_COUNT.
      CLEAR IT_ANBD.
      APPEND IT_ANBD.
    ELSE.
      CLEAR: WA_LINES.
      EXIT.
    ENDIF.
  ENDDO.

  DO C_LINE_COUNT TIMES .
    DESCRIBE TABLE IT_MONBD LINES WA_LINES.
    IF WA_LINES < C_LINE_COUNT.
      CLEAR IT_MONBD.
      APPEND IT_MONBD.
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
*&      Module  SELECT_YEAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SELECT_YEAR INPUT.
  PERFORM SELECT_YEAR.
ENDMODULE.                 " SELECT_YEAR  INPUT
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
*&---------------------------------------------------------------------*
*&      Form  READ_DATA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA2.
  DATA: WA_YEAR(2)   TYPE N,      "LIKE ZTPM_ANBD-AJAHR,
        WA_COUNT     LIKE T5A4A-DLYMO.

  CLEAR: IT_ZTPM_ANBD, IT_ZTPM_ANBD[],
         IT_ANBD, IT_ANBD[],
         IT_MONBD, IT_MONBD[].

  CLEAR : WA_AVRATE, WA_SUM_AVRATE, WA_OPTIME.

  PERFORM MAKE_SELECTION_ENTRY.

*** Rearrange Internal Table to Plat Structure
****  like month_01, month_02, month_03.....
  LOOP AT IT_SHOP.
    LOOP AT IT_YEAR.
      WA_YEAR = SY-TABIX.

      SELECT   *
               INTO  CORRESPONDING FIELDS OF TABLE IT_TEMP
               FROM  ZTPM_MONBD
               WHERE SHOP  = IT_SHOP-SHOP
               AND   AJAHR = IT_YEAR-AJAHR.

      CLEAR : WA_SUM_AVRATE, WA_COUNT.
*** GET OPERATION TIME...
      LOOP AT IT_TEMP.
        CLEAR : WA_OPTIME, WA_AVRATE.
        SELECT  SINGLE OPTIME INTO WA_OPTIME
                FROM  ZTPM_OPTIME
                WHERE AJAHR  = IT_TEMP-AJAHR
                AND   ZMONTH = IT_TEMP-ZMONTH
                AND   SHOP   = IT_TEMP-SHOP.
        IF NOT WA_OPTIME IS INITIAL.
          WA_AVRATE = IT_TEMP-ZDOWNTIME / WA_OPTIME.
          WA_SUM_AVRATE = WA_SUM_AVRATE + WA_AVRATE.
          WA_COUNT = WA_COUNT + 1.
        ELSE.
          WA_COUNT = WA_COUNT + 1.
        ENDIF.
      ENDLOOP.
      IF NOT WA_COUNT IS INITIAL.
        WA_AVRATE = WA_SUM_AVRATE / WA_COUNT * 100.
      ENDIF.

      PERFORM SET_IT_ANBD USING WA_YEAR
                                WA_AVRATE .
      CLEAR: WA_AVRATE.  CLEAR: WA_COUNT.
    ENDLOOP.

    MOVE: IT_SHOP-SHOP TO IT_ANBD-SHOP.
    SELECT SINGLE INNAM  INTO IT_ANBD-SHTXT
                  FROM T024I
                  WHERE INGRP = IT_SHOP-SHOP.
    APPEND IT_ANBD.
    CLEAR: WA_YEAR, IT_ANBD.
  ENDLOOP.

  SORT IT_ANBD BY SHOP.

  LOOP AT IT_SHOP.
    LOOP AT IT_RANGE.
      CLEAR: IT_RATE, IT_RATE[].
      CLEAR: ZTPM_MONBD, WA_AVRATE, WA_OPTIME.
      WA_MONTH = SY-TABIX.
      IF WA_MONTH = 11 OR WA_MONTH = 12.

        CLEAR: IT_TEMP_RATE, IT_TEMP_RATE[].

        CALL FUNCTION 'Z_FPM_CAL_BREAKDOWN_RATE_MON'
          EXPORTING
            I_MONTH       = IT_RANGE-ZMONTH
*             I_PLANT       =
            I_SHOP        = IT_SHOP-SHOP
            I_MAUEH       = 'MIN'
          TABLES
            T_RATE        = IT_TEMP_RATE
                  .
        READ TABLE IT_TEMP_RATE INDEX 1.
        IF SY-SUBRC EQ 0.
          MOVE: IT_TEMP_RATE-AVRATE TO WA_AVRATE.
          PERFORM SET_IT_MONBD USING WA_MONTH
                                      WA_AVRATE.
        ENDIF.
      ELSE.
*** Calculate Actual Monthly Breakdown rate...
        SELECT SINGLE  *
               FROM  ZTPM_MONBD
               WHERE SHOP   = IT_SHOP-SHOP
               AND   AJAHR  = IT_RANGE-ZMONTH(4)
               AND   ZMONTH = IT_RANGE-ZMONTH+4(2).
        IF SY-SUBRC EQ 0.
          SELECT  SINGLE OPTIME INTO WA_OPTIME
                  FROM  ZTPM_OPTIME
                  WHERE SHOP   = IT_SHOP-SHOP
                  AND   AJAHR  = IT_RANGE-ZMONTH(4)
                  AND   ZMONTH = IT_RANGE-ZMONTH+4(2).
          IF NOT WA_OPTIME IS INITIAL.
            WA_AVRATE = ZTPM_MONBD-ZDOWNTIME / WA_OPTIME * 100.
          ENDIF.
          PERFORM SET_IT_MONBD USING WA_MONTH
                                     WA_AVRATE.
        ENDIF.
      ENDIF.
    ENDLOOP.

*    MOVE: IT_SHOP-SHOP TO IT_MONBD-SHOP.
    APPEND IT_MONBD.
    CLEAR: WA_MONTH, IT_MONBD.
  ENDLOOP.

*** Append space fields to internal table for filling empty cells
*** Excel display empty cell "N/A"
  PERFORM FILL_EMPTY_CELL.

*  SORT IT_MONBD BY SHOP.

ENDFORM.                    " READ_DATA2
*&---------------------------------------------------------------------*
*&      Form  MAKE_SELECTION_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_SELECTION_ENTRY.

  DATA: WA_DATE      LIKE SY-DATUM,
        CAL_DATE     LIKE SY-DATUM,
        WA_COUNT     LIKE T5A4A-DLYMO.

  CLEAR: IT_SHOP, IT_SHOP[],
         IT_YEAR, IT_YEAR[],
         IT_RANGE, IT_RANGE[].

  CLEAR: WA_S_YEAR, WA_E_YEAR.

  IF S_SHOP-LOW EQ SPACE.
    CLEAR : S_SHOP, S_SHOP[].
  ENDIF.

  IF SY-DYNNR EQ '0101' AND S_SHOP-LOW NE SPACE.
    CLEAR : S_SHOP[].
    S_SHOP-SIGN = 'I'.
    S_SHOP-OPTION = 'EQ'.
    APPEND S_SHOP.
  ENDIF.


  SELECT  DISTINCT INGRP AS SHOP
          INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
          FROM  T024I
          WHERE INGRP IN S_SHOP.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-002.
    LEAVE TO SCREEN 0.
  ENDIF.

*** Make up Year range...
  IT_YEAR-AJAHR = P_ZMONTH(4).
  WA_E_YEAR = IT_YEAR-AJAHR.
  APPEND IT_YEAR.
  IT_YEAR-AJAHR = P_ZMONTH(4) - 1.
  APPEND IT_YEAR.
  IT_YEAR-AJAHR = P_ZMONTH(4) - 2.
  APPEND IT_YEAR.
  IT_YEAR-AJAHR = P_ZMONTH(4) - 3.
  APPEND IT_YEAR.
  WA_S_YEAR = IT_YEAR-AJAHR.
  SORT  IT_YEAR BY AJAHR. "DESCENDING.

*** make first day for select condition
  CONCATENATE  P_ZMONTH  '01' INTO WA_DATE.

*** make select entry (year/month)
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
    MOVE : CAL_DATE(6) TO IT_RANGE-ZMONTH.
    APPEND IT_RANGE.
    MOVE SY-INDEX TO WA_COUNT.
  ENDDO.

  SORT IT_RANGE BY ZMONTH ASCENDING.
ENDFORM.                    " MAKE_SELECTION_ENTRY
