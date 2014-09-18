*&---------------------------------------------------------------------*
*& Report  YRPM_REPORT_TEMP                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  YRPM_REPORT_TEMP   MESSAGE-ID DEMOOFFICEINTEGRATIO.

INCLUDE ZRPMOFFICE_TOP.
INCLUDE ZRPMOFFICE_CLASS_DEFINITION.
INCLUDE ZRPMOFFICE_CLASS_IMPLEMENT.

CONSTANTS: TRUE VALUE 1, FALSE VALUE 0.

DATA: FIRST_OPEN VALUE '1'.
DATA: OPEN_DOCUMENT(1).

DATA: FACTORY TYPE REF TO I_OI_DOCUMENT_FACTORY.
DATA: DOCUMENT TYPE REF TO I_OI_DOCUMENT_PROXY.
DATA: LINK_SERVER TYPE REF TO I_OI_LINK_SERVER.
DATA: BDS_INSTANCE TYPE REF TO CL_BDS_DOCUMENT_SET.

DATA: RETCODE TYPE T_OI_RET_STRING,
      DOCUMENT_TYPE(80) VALUE SOI_DOCTYPE_WORD97_DOCUMENT,
      DOCUMENT_FORMAT(80) TYPE C.
DATA: DOCUMENTS TYPE DOCUMENT_LIST.
DATA: DESCR TYPE DOCUMENT_DESCR.


DATA: DATA_TABLE TYPE SBDST_CONTENT,
      DATA_SIZE TYPE I, DOC_URL TYPE T_URL,
      HAS_CHANGED TYPE I,
      DELIMITER(1) TYPE C VALUE'/',
      DATA_TYPE(64) TYPE C,
      DATA_SUBTYPE(64) TYPE C,
      DOCUMENT_MIMETYPE TYPE BAPICOMPON-MIMETYPE.




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

  ENDMETHOD.

  METHOD CUSTOM_EVENT_HANDLER.

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: O1 TYPE REF TO C_EVENT_HANDLER.
  CREATE OBJECT O1.

  SET PF-STATUS 'MAIN0100'.
  SET TITLEBAR '001'.

  RETCODE = C_OI_ERRORS=>RET_OK.

  IF BDS_INSTANCE IS INITIAL.
    CREATE OBJECT BDS_INSTANCE.
  ENDIF.

  IF FACTORY IS INITIAL.
    CALL METHOD C_OI_FACTORY_CREATOR=>GET_DOCUMENT_FACTORY
                      IMPORTING FACTORY = FACTORY
                                RETCODE = RETCODE.
    IF RETCODE NE C_OI_ERRORS=>RET_OK. EXIT. ENDIF.

    CALL METHOD FACTORY->START_FACTORY
                        EXPORTING R3_APPLICATION_NAME =
                                            'SAP Basis'     "#EC NOTEXT
                                  REGISTER_ON_CLOSE_EVENT = 'X'
                                  REGISTER_ON_CUSTOM_EVENT = 'X'
                        IMPORTING RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    CALL METHOD FACTORY->GET_LINK_SERVER
                       IMPORTING LINK_SERVER = LINK_SERVER
                                 RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    CALL METHOD LINK_SERVER->START_LINK_SERVER
                      IMPORTING RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    CALL METHOD FACTORY->GET_DOCUMENT_PROXY
                      EXPORTING DOCUMENT_TYPE = DOCUMENT_TYPE
                      IMPORTING DOCUMENT_PROXY = DOCUMENT
                                RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    SET HANDLER C_EVENT_HANDLER=>CLOSE_EVENT_HANDLER FOR DOCUMENT.
    SET HANDLER C_EVENT_HANDLER=>CUSTOM_EVENT_HANDLER FOR DOCUMENT.

    PERFORM REFRESH_LINKS.

  ENDIF.

ENDMODULE.                             " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  refresh_links
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_LINKS.
  IF NOT LINK_SERVER IS INITIAL.

    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
             EXPORTING ITEM_NAME = 'Project Name11'
                       ITEM_VALUE  = 'project_name'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE.

    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
             EXPORTING ITEM_NAME = 'Topic11'
                       ITEM_VALUE  = 'project_topic'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE.
  ENDIF.
ENDFORM.                    " refresh_links
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CALL METHOD CL_GUI_CFW=>DISPATCH.

  CASE FCODE.
    WHEN 'EXIT'.                       "Zur?k
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
      IF NOT FACTORY IS INITIAL.
        CALL METHOD FACTORY->STOP_FACTORY IMPORTING RETCODE = RETCODE.
        FREE FACTORY.
      ENDIF.
      IF NOT BDS_INSTANCE IS INITIAL.
        FREE BDS_INSTANCE.
      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN 'SELECT'.
      IF NOT DOCUMENT IS INITIAL.
        PERFORM SAVE_DOCUMENT TABLES DATA_TABLE
                   USING 'X' 'X'
                   CHANGING DATA_SIZE DOCUMENT RETCODE.
      ENDIF.
      IF NOT FACTORY IS INITIAL.
        REFRESH DOCUMENTS.
        DESCR-DOCUMENT_NAME =
                     'Office Integration Description'(DO1).
        DESCR-DOCUMENT_ID = 'DEMOWORD97DOC'.
        APPEND DESCR TO DOCUMENTS.
        DESCR-DOCUMENT_NAME =
                     'Demo Document'(DO2).
        DESCR-DOCUMENT_ID = 'DEMOWORD97DOC2'.
        APPEND DESCR TO DOCUMENTS.

        CLEAR DOC_URL.
        PERFORM LOAD_DOCUMENT CHANGING
                                      DOCUMENT_TYPE
                                      DOCUMENT_FORMAT
                                      DOC_URL.

        IF NOT DOC_URL IS INITIAL.
          CALL METHOD DOCUMENT->OPEN_DOCUMENT
                                 EXPORTING DOCUMENT_URL = DOC_URL
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

          CALL METHOD DOCUMENT->EXECUTE_MACRO
                  EXPORTING MACRO_STRING = 'R3UpdateTables' "#EC NOTEXT
                  IMPORTING RETCODE = RETCODE.
        ELSE.
          MESSAGE E010.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATA_TABLE  text
*      -->P_0345   text
*      -->P_0346   text
*      <--P_DATA_SIZE  text
*      <--P_DOCUMENT  text
*      <--P_RETCODE  text
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
  DATA: DOC_CLASSNAME TYPE SBDST_CLASSNAME VALUE 'SOFFICEINTEGRATION',
        DOC_CLASSTYPE TYPE SBDST_CLASSTYPE VALUE 'OT',
        DOC_OBJECT_KEY TYPE SBDST_OBJECT_KEY VALUE 'SOFFICEINTEGRATION',
        DOC_MIMETYPE LIKE BAPICOMPON-MIMETYPE.


  CLEAR DOC_URL.

  WA_DOC_SIGNATURE-PROP_NAME = 'DESCRIPTION'.
  WA_DOC_SIGNATURE-PROP_VALUE = 'DEMOEXCELCHART1'.
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
