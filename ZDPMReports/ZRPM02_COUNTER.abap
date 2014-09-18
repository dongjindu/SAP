************************************************************************
* Program Name      : ZRPM02_COUNTER
* Author            : Myoung ho, Park
* Creation Date     : 2003.11.29.
* Specifications By :
* Pattern           : Report 1-1
* Development Request No :
* Addl Documentation:
* Description       : Breakdown Countermeasure report
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

REPORT  ZRPM02_COUNTER   MESSAGE-ID DEMOOFFICEINTEGRATIO.


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
*** Screen Structure...
TABLES: ZSPM_COMP,        "//Order Completetion Confirmation
        ZSPM_TIME_CONF,   "//Time Confirmation
        ZSPM_COUNTER.     "//Countermeasure

*** Data Select Table...
TABLES: AFRU,             "//Order completion confirmations
        ITOB,             "//PM technical objects
                          "//(EQUI, funcational location)
        QMUR,             "//Quality notification - causes
        QMMA,             "//Quality notification - activities.
        VIQMFE,           "//PM Notification - Item
        MKPF,             "//Header: Material Document
        MSEG.             "//Document Segment: Material

DATA : WA_ENAME LIKE PA0001-ENAME.  "//Equipment Personnel
"//(Empl./appl.name)
DATA : WA_FING LIKE T357-FING.    "//shop name(Plant section)
DATA : WA_KTEXT LIKE CRTX-KTEXT.  "//line name(Work center)
DATA : WA_NAME LIKE ADRP-NAME_TEXT. "//Reported by

DATA : BEGIN OF IT_COUNTER OCCURS 0.
        INCLUDE STRUCTURE ZSPM_COUNTER2.
DATA : END OF IT_COUNTER.

DATA : IT_TEMP_COUNTER LIKE ZSPM_COUNTER2 OCCURS 0.

*** for Long text....
*** SAPscript: Text Header
DATA : HEADLTX    LIKE THEAD.

*** Text Header Names
DATA: WA_TDNAME01 LIKE HEADLTX-TDNAME.
DATA: WA_TDNAME02 LIKE HEADLTX-TDNAME.
DATA: WA_TDNAME03 LIKE HEADLTX-TDNAME.
DATA: WA_TDNAME04 LIKE HEADLTX-TDNAME.

***  text internal tables
DATA: BEGIN OF IT_LTXTTAB01 OCCURS 100.
        INCLUDE STRUCTURE TLINE.
DATA: END OF IT_LTXTTAB01.

DATA: BEGIN OF IT_LTXTTAB02 OCCURS 100.
        INCLUDE STRUCTURE TLINE.
DATA: END OF IT_LTXTTAB02.

DATA: BEGIN OF IT_LTXTTAB03 OCCURS 100.
        INCLUDE STRUCTURE TLINE.
DATA: END OF IT_LTXTTAB03.

DATA: BEGIN OF IT_LTXTTAB04_01 OCCURS 100.
        INCLUDE STRUCTURE TLINE.
DATA: END OF IT_LTXTTAB04_01.

DATA: BEGIN OF IT_LTXTTAB04_02 OCCURS 100.
        INCLUDE STRUCTURE TLINE.
DATA: END OF IT_LTXTTAB04_02.

DATA: BEGIN OF IT_LTXTTAB04_03 OCCURS 100.
        INCLUDE STRUCTURE TLINE.
DATA: END OF IT_LTXTTAB04_03.

**** Temp text internal tables for LinkServer
DATA : IT_TEMP_LTXTTAB01 LIKE TLINE OCCURS 100.
DATA : IT_TEMP_LTXTTAB02 LIKE TLINE OCCURS 100.
DATA : IT_TEMP_LTXTTAB03 LIKE TLINE OCCURS 100.

DATA : IT_TEMP_LTXTTAB04_01 LIKE TLINE OCCURS 100.
DATA : IT_TEMP_LTXTTAB04_02 LIKE TLINE OCCURS 100.
DATA : IT_TEMP_LTXTTAB04_03 LIKE TLINE OCCURS 100.

FIELD-SYMBOLS: <TXTTAB> TYPE TEXT_LINE_TAB.
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

PARAMETER : P_AUFNR LIKE ZSPM_COMP-AUFNR MEMORY ID  ANR.

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
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

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
              EXPORTING CONTAINER_NAME = 'CONTAINER'.

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
****  Link Server name SUFFIX : 'COUNT'
    CALL METHOD LINK_SERVER->START_LINK_SERVER
                      EXPORTING LINK_SERVER_MODE =
*                                LINK_SERVER->LINK_SERVER_STANDARDNAME
                                LINK_SERVER->LINK_SERVER_CUSTOMNAME
                                SERVER_NAME_SUFFIX = 'COUNT'
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


*** Refresh Links ...
    PERFORM REFRESH_LINKS.
*** Open document...
    PERFORM OPEN_DOCUMENT.

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

  PERFORM READ_DATA.

  IF NOT LINK_SERVER IS INITIAL.

    PERFORM LINK_BREAKDOWN_LONG_TEXT.
    PERFORM LINK_COUNTERMEASURE_DETAIL.
    PERFORM LINK_COUNTERMEASURE_LONG_TEXT.

    PERFORM LINK_HEADER_INFO.
    PERFORM LINK_BREAKDOWN_DETAIL.

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
      LEAVE PROGRAM.
*      LEAVE TO TRANSACTION SY-TCODE.


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
        DOC_OBJECT_KEY TYPE SBDST_OBJECT_KEY VALUE 'COUNTER',
        DOC_MIMETYPE LIKE BAPICOMPON-MIMETYPE.


  CLEAR DOC_URL.

  WA_DOC_SIGNATURE-PROP_NAME = 'DESCRIPTION'.
  WA_DOC_SIGNATURE-PROP_VALUE = 'Breakdown Countermasure Report'.
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
                  EXCEPTIONS NOTHING_FOUND  = 1
                             ERROR_KPRO     = 2
                             INTERNAL_ERROR = 3
                             PARAMETER_ERROR = 4
                             NOT_AUTHORIZED  = 5
                             NOT_ALLOWED     = 6.
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
      LEAVE PROGRAM.
*      LEAVE TO TRANSACTION SY-TCODE.

    WHEN 'RW'.
      CLEAR: SY-UCOMM.
      PERFORM FREE_OBJECT.
      LEAVE PROGRAM.
*      LEAVE TO TRANSACTION SY-TCODE.

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

  CLEAR: ZSPM_COMP.
  PERFORM READ_HEADER_INFO.
  PERFORM READ_BREAKDOWN_DETAIL.
  PERFORM READ_BREAKDOWN_LONG_TEXT.
  PERFORM READ_COUNTERMEASURE_DETAIL.
  PERFORM READ_COUNTERMEASURE_LONG_TEXT.
  PERFORM FILL_EMPTY_CELL.

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
                                       OPEN_INPLACE = 'X'
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
*&      Form  READ_LONG_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LTXTTAB01  text
*      -->P_HEADLTX  text
*----------------------------------------------------------------------*
FORM READ_LONG_TEXT TABLES   PT_LTXTTAB STRUCTURE TLINE
                    USING    P_HEADLTX  LIKE THEAD.

  CALL FUNCTION 'READ_TEXT'
       EXPORTING
            ID        = P_HEADLTX-TDID
            LANGUAGE  = P_HEADLTX-TDSPRAS
            NAME      = P_HEADLTX-TDNAME
            OBJECT    = P_HEADLTX-TDOBJECT
       IMPORTING
            HEADER    = P_HEADLTX
       TABLES
            LINES     = PT_LTXTTAB
       EXCEPTIONS
            NOT_FOUND = 1.

ENDFORM.                    " READ_LONG_TEXT
*&---------------------------------------------------------------------*
*&      Form  LINK_HEADER_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LINK_HEADER_INFO.
*** Order Number
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = P_AUFNR
       IMPORTING
            OUTPUT = P_AUFNR.

  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
           EXPORTING ITEM_NAME = 'Order_No'
                     ITEM_VALUE  = P_AUFNR
                     NO_FLUSH    = 'X'
           IMPORTING RETCODE = RETCODE.

*** Breakdown_Period
  DATA : WA_AUSVN(10).
  DATA : WA_AUSBS(10).
  DATA : WA_PERIOD(25).

  WRITE : ZSPM_COMP-AUSVN TO WA_AUSVN MM/DD/YYYY.
  WRITE : ZSPM_COMP-AUSBS TO WA_AUSBS MM/DD/YYYY.
  CONCATENATE WA_AUSVN ' - ' WA_AUSBS INTO WA_PERIOD.

  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Period'
                        ITEM_VALUE  = WA_PERIOD
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

**** Breakdwon Time
  DATA : WA_AUZTV(8).
  DATA : WA_AUZTB(8).
  DATA : WA_TIME(20).

  WRITE ZSPM_COMP-AUZTV TO WA_AUZTV. "TIME ZONE SY-TZONE.
  WRITE ZSPM_COMP-AUZTB TO WA_AUZTB. "TIME ZONE SY-TZONE.
  CONCATENATE WA_AUZTV ' - ' WA_AUZTB INTO WA_TIME.

  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Time'
                        ITEM_VALUE  = WA_TIME
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

****  Breakdown Time
  DATA: WA_DOWN(25),
        WA_AUSZT(22).

  CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
       EXPORTING
            CHAR_UNIT       = ZSPM_COMP-MAUEH
            DECIMALS        = 0
            EXPONENT        = 0
            FLTP_VALUE_SI   = ZSPM_COMP-AUSZT
            INDICATOR_VALUE = 'X'
            MASC_SYMBOL     = ' '
       IMPORTING
            CHAR_VALUE      = WA_AUSZT.

*  WRITE ZSPM_COMP-AUSZT TO WA_AUSZT EXPONENT 0 DECIMALS 0.
  CONCATENATE WA_AUSZT ' ' ZSPM_COMP-MAUEH INTO WA_DOWN.
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Down_Time'
                        ITEM_VALUE  = WA_DOWN
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

**** Line Down Time(Actual duration)
  DATA: WA_DURATION(15),
        WA_IDAUR(7).
  WRITE ZSPM_COMP-IDAUR TO WA_IDAUR UNIT ZSPM_COMP-IDAUE.
  CONCATENATE WA_IDAUR ' - ' ZSPM_COMP-IDAUE INTO WA_DURATION.

  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Duration'
                        ITEM_VALUE  = WA_DURATION
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

*** Title
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Title'
                        ITEM_VALUE  = ZSPM_COMP-KTEXT
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.
*** Shop/Line
  DATA: WA_SHOP_LINE(30).
  CONCATENATE  WA_FING ' / ' WA_KTEXT INTO WA_SHOP_LINE.

  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Shop_Line'
* changed by 100565 07_10_04
*                        ITEM_VALUE  = WA_SHOP_LINE
                        ITEM_VALUE  = WA_Fing
* end changed by 100565 07_10_04
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.
**** Equipment
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = ZSPM_COMP-EQUNR
       IMPORTING
            OUTPUT = ZSPM_COMP-EQUNR.

  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Equipment'
                        ITEM_VALUE  = ZSPM_COMP-EQUNR
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

**** Equipment Name
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Equipment_Name'
                        ITEM_VALUE  = ITOB-SHTXT
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

*** Equipment Personnel
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Equip_Personnel'
                        ITEM_VALUE  = WA_ENAME
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

*** Executor
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Executor'
                        ITEM_VALUE  = ZSPM_COMP-QMNAM
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

**** Process(Room)
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Process'
                        ITEM_VALUE  = ITOB-MSGRP
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

**** Dept.name (Shop)
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Dept'
                        ITEM_VALUE  = WA_FING
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.
****change by 100565 07_10_04
CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Shop_Name'
                        ITEM_VALUE  = WA_FING
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Line_Name'
                        ITEM_VALUE  = WA_KTEXT
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.


****endchange by 100565 07_10_04


**** Report by
*  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*              EXPORTING ITEM_NAME = 'User'
*                        ITEM_VALUE  = WA_NAME
*                        NO_FLUSH    = 'X'
*              IMPORTING RETCODE = RETCODE.

**** Report Date
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'report_date'
                        ITEM_VALUE  = SY-DATUM
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

ENDFORM.                    " LINK_HEADER_INFO
*&---------------------------------------------------------------------*
*&      Form  LINK_BREAKDOWN_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LINK_BREAKDOWN_DETAIL.
***Damage Code
*  ZSPM_COMP-FECOD = '01'.
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Damage_Code'
                        ITEM_VALUE  = ZSPM_COMP-FECOD
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

**** Damage Code text
shift ZSPM_COMP-TXTCDFE by 4 places right.
*  ZSPM_COMP-TXTCDOT = 'Damage_Text'.
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Damage_Text'
                        ITEM_VALUE  = ZSPM_COMP-TXTCDFE
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

**** Cause Code
*  ZSPM_COMP-URCOD = 'Cause_Code'.
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Cause_Code'
                        ITEM_VALUE  = ZSPM_COMP-URCOD
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

**** Cause Code text
shift ZSPM_COMP-TXTCDUR by 4 places right.
*  ZSPM_COMP-TXTCDUR = 'Cause_Text'.
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Cause_Text'
                        ITEM_VALUE  = ZSPM_COMP-TXTCDUR
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

***** Activity Code
*  ZSPM_COMP-MNCOD = 'Activity_Code'.
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Activity_Code'
                        ITEM_VALUE  = ZSPM_COMP-MNCOD
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

***** Activity Code text
shift ZSPM_COMP-TXTCDMA by 4 places right.
*  ZSPM_COMP-TXTCDMA = 'Activity_Text'.
  CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME = 'Activity_Text'
                        ITEM_VALUE  = ZSPM_COMP-TXTCDMA
                        NO_FLUSH    = ' '
              IMPORTING RETCODE = RETCODE.

ENDFORM.                    " LINK_BREAKDOWN_DETAIL
*&---------------------------------------------------------------------*
*&      Form  link_breakdown_long_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LINK_BREAKDOWN_LONG_TEXT.
****** Damage Long text
* CHANGED BY 100565
loop at IT_LTXTTAB01.
IF SY-TABIX = '1'.
SHIFT IT_LTXTTAB01-TDLINE BY 4 PLACES RIGHT.
  MODIFY IT_LTXTTAB01.
  ENDIF.
  endloop.
*sort IT_LTXTTAB01 by tdline descending.

  IT_TEMP_LTXTTAB01[] = IT_LTXTTAB01[].


  CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
           EXPORTING ITEM_NAME   = 'Damage_Long'
                     ITEM_TITLE  = 'Damage Long text'
                     DDIC_NAME   = 'TLINE'
                     NO_FLUSH    = 'X'
           IMPORTING RETCODE = RETCODE
           CHANGING  DATA_TABLE   = IT_TEMP_LTXTTAB01.

****** Cause Long text
*  MOVE 'Cause Long text' TO IT_LTXTTAB02-TDLINE.
*  APPEND IT_LTXTTAB02.

* CHANGED BY 100565
loop at IT_LTXTTAB02.
IF SY-TABIX = '1'.
SHIFT IT_LTXTTAB02-TDLINE BY 4 PLACES RIGHT.
  MODIFY IT_LTXTTAB02.
  ENDIF.
  endloop.


  IT_TEMP_LTXTTAB02[] = IT_LTXTTAB02[].
  CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
           EXPORTING ITEM_NAME   = 'Cause_Long'
                     ITEM_TITLE  = 'Cause Long text'
                     DDIC_NAME   = 'TLINE'
                     NO_FLUSH    = 'X'
           IMPORTING RETCODE = RETCODE
           CHANGING  DATA_TABLE   = IT_TEMP_LTXTTAB02.

****** Activity Long text
*  MOVE 'Activity Long text1' TO IT_LTXTTAB03-TDLINE.
*  APPEND IT_LTXTTAB03.
*  MOVE 'Activity Long text2' TO IT_LTXTTAB03-TDLINE.
*  APPEND IT_LTXTTAB03.

* CHANGED BY 100565
loop at IT_LTXTTAB03.
IF SY-TABIX = '1'.
SHIFT IT_LTXTTAB03-TDLINE BY 4 PLACES RIGHT.
  MODIFY IT_LTXTTAB03.
  ENDIF.
  endloop.


  IT_TEMP_LTXTTAB03[] = IT_LTXTTAB03[].
  CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
           EXPORTING ITEM_NAME   = 'Activity_Long'
                     ITEM_TITLE  = 'Activity Long text'
                     DDIC_NAME   = 'TLINE'
                     NO_FLUSH    = 'X'
           IMPORTING RETCODE = RETCODE
           CHANGING  DATA_TABLE   = IT_TEMP_LTXTTAB03.

ENDFORM.                    " link_breakdown_long_text
*&---------------------------------------------------------------------*
*&      Form  LINK_COUNTERMEASURE_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LINK_COUNTERMEASURE_DETAIL.

  LOOP AT IT_COUNTER WHERE AUFNR NE SPACE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
         EXPORTING
              INPUT  = IT_COUNTER-AUFNR
         IMPORTING
              OUTPUT = IT_COUNTER-AUFNR.
    MODIFY IT_COUNTER TRANSPORTING AUFNR.
  ENDLOOP.

  IT_TEMP_COUNTER[] = IT_COUNTER[].

  CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
           EXPORTING ITEM_NAME   = 'Counter'
                     ITEM_TITLE  = 'Countermeasure info'
                     DDIC_NAME   = 'ZSPM_COUNTER2'
                     NO_FLUSH    = 'X'
           IMPORTING RETCODE = RETCODE
           CHANGING  DATA_TABLE   = IT_TEMP_COUNTER.
ENDFORM.                    " LINK_COUNTERMEASURE_DETAIL
*&---------------------------------------------------------------------*
*&      Form  LINK_COUNTERMEASURE_LONG_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LINK_COUNTERMEASURE_LONG_TEXT.
****** Countermeasure Long text
***** # 1
*  MOVE 'Countermeasure Long text #1' TO IT_LTXTTAB04_1-TDLINE.
*  APPEND IT_LTXTTAB04_1.

  IT_TEMP_LTXTTAB04_01[] = IT_LTXTTAB04_01[].
  CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
           EXPORTING ITEM_NAME   = 'Counter_Long1'
                     ITEM_TITLE  = 'Countermeasure Long text #1'
                     DDIC_NAME   = 'TLINE'
                     NO_FLUSH    = 'X'
           IMPORTING RETCODE = RETCODE
           CHANGING  DATA_TABLE   = IT_TEMP_LTXTTAB04_01.
***** # 2
*  MOVE 'Countermeasure Long text #2' TO IT_LTXTTAB04_2-TDLINE.
*  APPEND IT_LTXTTAB04_2.

  IT_TEMP_LTXTTAB04_02[] = IT_LTXTTAB04_02[].
  CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
           EXPORTING ITEM_NAME   = 'Counter_Long2'
                     ITEM_TITLE  = 'Countermeasure Long text #2'
                     DDIC_NAME   = 'TLINE'
                     NO_FLUSH    = 'X'
           IMPORTING RETCODE = RETCODE
           CHANGING  DATA_TABLE   = IT_TEMP_LTXTTAB04_02.
***** # 3
*  MOVE 'Countermeasure Long text #3' TO IT_LTXTTAB04_3-TDLINE.
*  APPEND IT_LTXTTAB04_3.

  IT_TEMP_LTXTTAB04_03[] = IT_LTXTTAB04_03[].
  CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
           EXPORTING ITEM_NAME   = 'Counter_Long3'
                     ITEM_TITLE  = 'Countermeasure Long text #3'
                     DDIC_NAME   = 'TLINE'
                     NO_FLUSH    = 'X'
           IMPORTING RETCODE = RETCODE
           CHANGING  DATA_TABLE   = IT_TEMP_LTXTTAB04_03.
ENDFORM.                    " LINK_COUNTERMEASURE_LONG_TEXT
*&---------------------------------------------------------------------*
*&      Form  READ_HEADER_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_HEADER_INFO.
**** Read Order detail info
**** Check Notification & Read Description
  SELECT SINGLE A~QMNUM B~KTEXT A~AUSVN A~AUSBS
                A~AUZTV A~AUZTB A~AUSZT A~MAUEH
                A~EQUNR A~QMNAM
         INTO  (ZSPM_COMP-QMNUM, ZSPM_COMP-KTEXT,
                ZSPM_COMP-AUSVN, ZSPM_COMP-AUSBS,
                ZSPM_COMP-AUZTV, ZSPM_COMP-AUZTB,
                ZSPM_COMP-AUSZT, ZSPM_COMP-MAUEH,
                ZSPM_COMP-EQUNR, ZSPM_COMP-QMNAM)
                         FROM   VIQMEL AS A
                                INNER JOIN AUFK AS B
                                      ON A~AUFNR = B~AUFNR
                         WHERE  A~AUFNR = P_AUFNR.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-001 ZSPM_COMP-AUFNR TEXT-M01.
  ELSE.
**** Line Down Time..(Actual duration)
    CLEAR : AFRU.
    SELECT SINGLE IDAUR IDAUE
           INTO  (ZSPM_COMP-IDAUR, ZSPM_COMP-IDAUE)
           FROM  AFRU
           WHERE AUFNR = P_AUFNR
           AND   RMZHL = '00000001'.

**** Equipment master data
    CLEAR : ITOB.
    SELECT SINGLE *
           FROM  ITOB
           WHERE EQUNR = ZSPM_COMP-EQUNR.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE FING
             INTO WA_FING
             FROM T357
             WHERE BEBER = ITOB-BEBER.

      SELECT SINGLE KTEXT
             INTO WA_KTEXT
             FROM CRTX
             WHERE OBJTY  = 'A'
             AND    OBJID = ITOB-WKCTR.

      SELECT SINGLE  B~ENAME
             INTO  WA_ENAME
             FROM  IHPA AS A
                   INNER JOIN PA0001 AS B
                   ON A~PARNR = B~PERNR
             WHERE A~OBJNR = ITOB-OBJNR.
    ENDIF.
  ENDIF.
*** Reported by
  CLEAR: WA_NAME.
  SELECT SINGLE B~NAME_TEXT
         INTO WA_NAME
         FROM USR21 AS A
              INNER JOIN ADRP AS B
              ON A~PERSNUMBER = B~PERSNUMBER
         WHERE A~BNAME = SY-UNAME.

ENDFORM.                    " READ_HEADER_INFO
*&---------------------------------------------------------------------*
*&      Form  read_breakdown_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_BREAKDOWN_DETAIL.
  CLEAR: QMUR, QMMA, VIQMFE.
**** Read Object Part Code , Damage & Notification Item Short Text
  SELECT SINGLE  POSNR OTEIL  FECOD FETXT
                 INTO (ZSPM_COMP-POSNR, ZSPM_COMP-OTEIL,
                       ZSPM_COMP-FECOD, ZSPM_COMP-FETXT)
                 FROM VIQMFE
                 WHERE QMNUM = ZSPM_COMP-QMNUM.
  IF SY-SUBRC EQ 0.
**** Read Object Part Code text
    IF NOT ZSPM_COMP-OTEIL IS INITIAL.
      CALL FUNCTION 'QPK1_CODE_TEXT'
           EXPORTING
                I_KATALOGART = 'B'
                I_CODEGRUPPE = 'PM01'
                I_CODE       = ZSPM_COMP-OTEIL
           IMPORTING
                E_TEXT       = ZSPM_COMP-TXTCDOT.

    ENDIF.
**** Read Damage Code text
    IF NOT ZSPM_COMP-FECOD IS INITIAL.
      CALL FUNCTION 'QPK1_CODE_TEXT'
           EXPORTING
                I_KATALOGART = 'C'
                I_CODEGRUPPE = 'PM01'
                I_CODE       = ZSPM_COMP-FECOD
           IMPORTING
                E_TEXT       = ZSPM_COMP-TXTCDFE.
    ENDIF.
  ENDIF.
**** Read Cause Code & Short Text for Cause Code
  SELECT SINGLE FENUM URNUM URCOD URTXT
         INTO  (ZSPM_COMP-FENUM,
                ZSPM_COMP-URNUM,
                ZSPM_COMP-URCOD,
                ZSPM_COMP-URTXT)
         FROM   QMUR
         WHERE  QMNUM = ZSPM_COMP-QMNUM.
  IF SY-SUBRC EQ 0.
**** Read Cause Code text
    CALL FUNCTION 'QPK1_CODE_TEXT'
         EXPORTING
              I_KATALOGART = '5'
              I_CODEGRUPPE = 'PM01'
              I_CODE       = ZSPM_COMP-URCOD
         IMPORTING
              E_TEXT       = ZSPM_COMP-TXTCDUR.
  ENDIF.
***** Activity Code & Short Text for Activity Code
  SELECT SINGLE MANUM MNCOD MATXT
         INTO  (ZSPM_COMP-MANUM,
                ZSPM_COMP-MNCOD,
                ZSPM_COMP-MATXT)
         FROM   QMMA
         WHERE  QMNUM = ZSPM_COMP-QMNUM.
  IF SY-SUBRC EQ 0.
***** Activity Code text
    CALL FUNCTION 'QPK1_CODE_TEXT'
         EXPORTING
              I_KATALOGART = 'A'
              I_CODEGRUPPE = 'PM01'
              I_CODE       = ZSPM_COMP-MNCOD
         IMPORTING
              E_TEXT       = ZSPM_COMP-TXTCDMA.
  ENDIF.

ENDFORM.                    " read_breakdown_detail
*&---------------------------------------------------------------------*
*&      Form  READ_BREAKDOWN_LONG_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_BREAKDOWN_LONG_TEXT.
**** Read Damage Long text
  CONCATENATE ZSPM_COMP-QMNUM
              ZSPM_COMP-FENUM
              INTO WA_TDNAME01.

  HEADLTX-TDID    = 'LTXT'.
  HEADLTX-TDSPRAS = 'E'.
  HEADLTX-TDNAME  = WA_TDNAME01.
  HEADLTX-TDOBJECT = 'QMFE'.
  HEADLTX-TDLINESIZE = 072.

  PERFORM READ_LONG_TEXT TABLES IT_LTXTTAB01
                         USING  HEADLTX.

  IF IT_LTXTTAB01 IS INITIAL.
    PERFORM READ_DAMAGE_SHORT_TEXT  TABLES IT_LTXTTAB01
                                    USING  ZSPM_COMP-QMNUM
                                           ZSPM_COMP-FENUM.
  ENDIF.

**** Read Cause Long text
  CONCATENATE ZSPM_COMP-QMNUM
              ZSPM_COMP-FENUM
              ZSPM_COMP-URNUM
              INTO WA_TDNAME02.

  HEADLTX-TDID    = 'LTXT'.
  HEADLTX-TDSPRAS = 'E'.
  HEADLTX-TDNAME  = WA_TDNAME02.
  HEADLTX-TDOBJECT = 'QMUR'.
  HEADLTX-TDLINESIZE = 072.

  PERFORM READ_LONG_TEXT TABLES IT_LTXTTAB02
                        USING  HEADLTX.

  IF IT_LTXTTAB02 IS INITIAL.
    PERFORM READ_CAUSE_SHORT_TEXT   TABLES IT_LTXTTAB02
                                    USING  ZSPM_COMP-QMNUM
                                           ZSPM_COMP-FENUM.
  ENDIF.

***** Activity Long text
  CONCATENATE ZSPM_COMP-QMNUM
              ZSPM_COMP-MANUM
              INTO WA_TDNAME03.

  HEADLTX-TDID    = 'LTXT'.
  HEADLTX-TDSPRAS = 'E'.
  HEADLTX-TDNAME  = WA_TDNAME03.
  HEADLTX-TDOBJECT = 'QMMA'.
  HEADLTX-TDLINESIZE = 072.

  PERFORM READ_LONG_TEXT TABLES IT_LTXTTAB03
                        USING  HEADLTX.

  IF IT_LTXTTAB03 IS INITIAL.
    PERFORM READ_ACTIVITY_SHORT_TEXT  TABLES IT_LTXTTAB03
                                      USING  ZSPM_COMP-QMNUM
                                             ZSPM_COMP-FENUM.
  ENDIF.
ENDFORM.                    " READ_BREAKDOWN_LONG_TEXT
*&---------------------------------------------------------------------*
*&      Form  READ_COUNTERMEASURE_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_COUNTERMEASURE_DETAIL.

  SELECT  A~QMNUM B~AUFNR B~GLTRP
          INTO CORRESPONDING FIELDS OF TABLE IT_COUNTER
          FROM  VIQMEL AS A
                INNER JOIN AFKO AS B
                ON A~AUFNR = B~AUFNR
          WHERE A~QWRNUM  = ZSPM_COMP-QMNUM.


*  SELECT  VORNR LTXA1
*      INTO CORRESPONDING FIELDS OF TABLE IT_OPERATION
*      FROM CAUFV AS A
*           INNER JOIN AFVC AS B
*           ON  A~AUFPL = B~AUFPL
*      WHERE A~AUFNR = IT_COUNTER-AUFNR.

ENDFORM.                    " READ_COUNTERMEASURE_DETAIL
*&---------------------------------------------------------------------*
*&      Form  READ_COUNTERMEASURE_LONG_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_COUNTERMEASURE_LONG_TEXT.
  DATA: WA_TAB_NAME(20),
        WA_TABIX(2) TYPE N.

  DATA: WA_AUFPL LIKE AFVC-AUFPL,
        WA_APLZL LIKE AFVC-APLZL.

  LOOP AT IT_COUNTER.
    IF SY-TABIX > 3. EXIT. ENDIF.
    WA_TABIX = SY-TABIX.


    SELECT SINGLE  A~AUFPL B~APLZL
           INTO (WA_AUFPL, WA_APLZL)
           FROM CAUFV  AS A
                 INNER JOIN AFVC AS B
                 ON A~AUFPL = B~AUFPL
           WHERE  A~AUFNR =  IT_COUNTER-AUFNR.

    CALL FUNCTION 'CO_ZK_TEXTKEY_AFVG'
         EXPORTING
              APLZL = WA_APLZL
              AUFPL = WA_AUFPL
         IMPORTING
              LTSCH = WA_TDNAME04.


    HEADLTX-TDID       = 'AVOT'.
    HEADLTX-TDSPRAS    = 'E'.
    HEADLTX-TDNAME     = WA_TDNAME04.
    HEADLTX-TDOBJECT   = 'AUFK'.
    HEADLTX-MANDT      = SY-MANDT.
    HEADLTX-TDLINESIZE = 072.
*    HEADLTX-TDMACODE1  = 'IW22SAPLIQS0'.
    HEADLTX-TDFORM     = 'SYSTEM'.

*    HEADLTX-TDID       = 'LTXT'.
*    HEADLTX-TDSPRAS    = 'E'.
*    HEADLTX-TDNAME     = WA_TDNAME04.
*    HEADLTX-TDOBJECT   = 'QMSM'.
*    HEADLTX-TDLINESIZE = 072.

*    CONCATENATE 'IT_LTXTTAB04_' WA_TABIX INTO WA_TAB_NAME.
*    ASSIGN (WA_TAB_NAME) TO <TXTTAB>.
*    PERFORM READ_LONG_TEXT  TABLES <TXTTAB>
*                            USING  HEADLTX.

    CASE SY-TABIX.
      WHEN 01.
        PERFORM READ_LONG_TEXT  TABLES IT_LTXTTAB04_01
                                USING  HEADLTX.

        IF IT_LTXTTAB04_01 IS INITIAL.
          PERFORM READ_SHORT_TEXT  TABLES IT_LTXTTAB04_01
                                   USING  WA_AUFPL.
        ENDIF.
      WHEN 02.
        PERFORM READ_LONG_TEXT  TABLES IT_LTXTTAB04_02
                                USING  HEADLTX.
        IF IT_LTXTTAB04_02 IS INITIAL.
          PERFORM READ_SHORT_TEXT  TABLES IT_LTXTTAB04_02
                                   USING  WA_AUFPL.
        ENDIF.
      WHEN 03.
        PERFORM READ_LONG_TEXT  TABLES IT_LTXTTAB04_03
                                USING  HEADLTX.
        IF IT_LTXTTAB04_03 IS INITIAL.
          PERFORM READ_SHORT_TEXT  TABLES IT_LTXTTAB04_03
                                   USING  WA_AUFPL.
        ENDIF.
    ENDCASE.

  ENDLOOP.
ENDFORM.                    " READ_COUNTERMEASURE_LONG_TEXT
*&---------------------------------------------------------------------*
*&      Form  FILL_EMPTY_CELL
*&---------------------------------------------------------------------*
*  Append space fields to internal table for filling empty cells
*  Excel display empty cell "N/A"
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_EMPTY_CELL.
  DATA: WA_LINES TYPE I.

**** for Damage Long text (max 2 lines)
  DO 2 TIMES .
    DESCRIBE TABLE IT_LTXTTAB01 LINES WA_LINES.
    IF WA_LINES < 2.
      CLEAR IT_LTXTTAB01.
      APPEND IT_LTXTTAB01.
    ELSE.
      CLEAR: WA_LINES.
      EXIT.
    ENDIF.
  ENDDO.

****for Cause Long text (max 2 lines)
  DO 2 TIMES .
    DESCRIBE TABLE IT_LTXTTAB02 LINES WA_LINES.
    IF WA_LINES < 2.
      CLEAR IT_LTXTTAB02.
      APPEND IT_LTXTTAB02.
    ELSE.
      CLEAR: WA_LINES.
      EXIT.
    ENDIF.
  ENDDO.

****for Activity Long text (max 4 lines)
  DO 4 TIMES .
    DESCRIBE TABLE IT_LTXTTAB03 LINES WA_LINES.
    IF WA_LINES < 4.
      CLEAR IT_LTXTTAB03.
      APPEND IT_LTXTTAB03.
    ELSE.
      CLEAR: WA_LINES.
      EXIT.
    ENDIF.
  ENDDO.

****for Countermeasure Long text 1 (max 3 lines)
  DO 4 TIMES .
    DESCRIBE TABLE IT_LTXTTAB04_01 LINES WA_LINES.
    IF WA_LINES < 3.
      CLEAR IT_LTXTTAB04_01.
      APPEND IT_LTXTTAB04_01.
    ELSE.
      CLEAR: WA_LINES.
      EXIT.
    ENDIF.
  ENDDO.

****for Countermeasure Long text 2 (max 3 lines)
  DO 4 TIMES .
    DESCRIBE TABLE IT_LTXTTAB04_02 LINES WA_LINES.
    IF WA_LINES < 3.
      CLEAR IT_LTXTTAB04_02.
      APPEND IT_LTXTTAB04_02.
    ELSE.
      CLEAR: WA_LINES.
      EXIT.
    ENDIF.
  ENDDO.

****for Countermeasure Long text 3 (max 3 lines)
  DO 4 TIMES .
    DESCRIBE TABLE IT_LTXTTAB04_03 LINES WA_LINES.
    IF WA_LINES < 3.
      CLEAR IT_LTXTTAB04_03.
      APPEND IT_LTXTTAB04_03.
    ELSE.
      CLEAR: WA_LINES.
      EXIT.
    ENDIF.
  ENDDO.

  DO 3 TIMES .
    DESCRIBE TABLE IT_COUNTER LINES WA_LINES.
    IF WA_LINES < 3.
      CLEAR IT_COUNTER.
      APPEND IT_COUNTER.
    ELSE.
      CLEAR: WA_LINES.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " FILL_EMPTY_CELL
*&---------------------------------------------------------------------*
*&      Form  READ_SHORT_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LTXTTAB04_01  text
*      -->P_WA_AUFPL  text
*      -->P_ENDIF  text
*----------------------------------------------------------------------*
FORM READ_SHORT_TEXT TABLES   PT_LTXTTAB STRUCTURE TLINE
                     USING    P_AUFPL.

  SELECT SINGLE  LTXA1 INTO PT_LTXTTAB-TDLINE
                 FROM AFVC WHERE AUFPL = P_AUFPL.
  IF SY-SUBRC EQ 0.
    APPEND PT_LTXTTAB.
  ENDIF.
ENDFORM.                    " READ_SHORT_TEXT
*&---------------------------------------------------------------------*
*&      Form  READ_DAMAGE_SHORT_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LTXTTAB01  text
*      -->P_WA_AUFPL  text
*----------------------------------------------------------------------*
FORM READ_DAMAGE_SHORT_TEXT TABLES  PT_LTXTTAB STRUCTURE TLINE
                            USING    P_QMNUM
                                     P_FENUM.

  SELECT SINGLE  FETXT  INTO PT_LTXTTAB-TDLINE
                 FROM  VIQMFE
                 WHERE QMNUM = P_QMNUM
                 AND   FENUM = P_FENUM.
  IF SY-SUBRC EQ 0.
    APPEND PT_LTXTTAB.
  ENDIF.
ENDFORM.                    " READ_DAMAGE_SHORT_TEXT
*&---------------------------------------------------------------------*
*&      Form  READ_CAUSE_SHORT_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LTXTTAB02  text
*      -->P_ZSPM_COMP_QMNUM  text
*      -->P_ZSPM_COMP_FENUM  text
*----------------------------------------------------------------------*
FORM READ_CAUSE_SHORT_TEXT TABLES   PT_LTXTTAB STRUCTURE TLINE
                            USING    P_QMNUM
                                     P_FENUM.

  SELECT SINGLE  URTXT  INTO PT_LTXTTAB-TDLINE
                 FROM  QMUR
                 WHERE QMNUM = P_QMNUM
                 AND   FENUM = P_FENUM.
  IF SY-SUBRC EQ 0.
    APPEND PT_LTXTTAB.
  ENDIF.
ENDFORM.                    " READ_CAUSE_SHORT_TEXT
*&---------------------------------------------------------------------*
*&      Form  READ_ACTIVITY_SHORT_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LTXTTAB03  text
*      -->P_ZSPM_COMP_QMNUM  text
*      -->P_ZSPM_COMP_FENUM  text
*----------------------------------------------------------------------*
FORM READ_ACTIVITY_SHORT_TEXT TABLES   PT_LTXTTAB STRUCTURE TLINE
                              USING    P_QMNUM
                                       P_FENUM.

  SELECT SINGLE  MATXT  INTO PT_LTXTTAB-TDLINE
                 FROM  QMMA
                 WHERE QMNUM = P_QMNUM
                 AND   FENUM = P_FENUM.
  IF SY-SUBRC EQ 0.
    APPEND PT_LTXTTAB.
  ENDIF.
ENDFORM.                    " READ_ACTIVITY_SHORT_TEXT
