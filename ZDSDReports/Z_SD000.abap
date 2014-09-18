***INCLUDE Z_SD000 .


DATA: FACTORY TYPE REF TO I_OI_DOCUMENT_FACTORY.
DATA: DOCUMENT TYPE REF TO I_OI_DOCUMENT_PROXY.
DATA: RETCODE TYPE T_OI_RET_STRING.

DATA: DOC_TABLE LIKE W3MIME OCCURS 0.
DATA: DOC_SIZE TYPE I.
DATA: DOC_TYPE(80) VALUE SOI_DOCTYPE_WORD97_DOCUMENT.
DATA: DOC_FORMAT(80) TYPE C.

DATA: LINK_SERVER TYPE REF TO I_OI_LINK_SERVER.

DATA: IS_CLOSED TYPE I.

*&---------------------------------------------------------------------*
*&      DEFINE M_LINK_TAB
*&---------------------------------------------------------------------*
DEFINE M_LINK_TAB.
  CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
           EXPORTING ITEM_NAME = &1
           IMPORTING RETCODE = RETCODE
           CHANGING  DATA_TABLE = &2.
  CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.
END-OF-DEFINITION.                              " M_LINK_TAB
*&---------------------------------------------------------------------*
*&      DEFINE  OPEN_DOC
*&---------------------------------------------------------------------*
DEFINE OPEN_DOC.
* &1 - Object ID Name.

  CALL FUNCTION 'SAP_OI_LOAD_MIME_DATA'
       EXPORTING
            OBJECT_ID        = &1
       IMPORTING
            DATA_SIZE        = DOC_SIZE
            DOCUMENT_FORMAT  = DOC_FORMAT
            DOCUMENT_TYPE    = DOC_TYPE
       TABLES
            DATA_TABLE       = DOC_TABLE
       EXCEPTIONS
            OBJECT_NOT_FOUND = 1
            INTERNAL_ERROR   = 2
            OTHERS           = 3.

  IF SY-SUBRC NE 0.            "
    MESSAGE ID SY-MSGID TYPE 'E' NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF DOC_SIZE NE 0.

    CALL METHOD FACTORY->GET_DOCUMENT_PROXY
                      EXPORTING DOCUMENT_TYPE = DOC_TYPE
                      IMPORTING DOCUMENT_PROXY = DOCUMENT
                                RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    CALL METHOD DOCUMENT->OPEN_DOCUMENT_FROM_TABLE
                      EXPORTING DOCUMENT_TABLE = DOC_TABLE[]
                                DOCUMENT_SIZE  = DOC_SIZE
                      IMPORTING RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

  ELSE.
    MESSAGE E016(PN) WITH 'no document'.
  ENDIF.

END-OF-DEFINITION.                              " OPEN_DOC
*&---------------------------------------------------------------------*
*&      Form  INIT_FACTORY
*&---------------------------------------------------------------------*
FORM INIT_FACTORY.

  IF FACTORY IS INITIAL.
    CALL METHOD C_OI_FACTORY_CREATOR=>GET_DOCUMENT_FACTORY
                      IMPORTING FACTORY = FACTORY
                                RETCODE = RETCODE.
    IF RETCODE NE C_OI_ERRORS=>RET_OK. EXIT. ENDIF.
    CALL METHOD FACTORY->START_FACTORY
                     EXPORTING R3_APPLICATION_NAME =
                                         'SOHEADER'
                     IMPORTING RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    CALL METHOD FACTORY->GET_LINK_SERVER
                       IMPORTING LINK_SERVER = LINK_SERVER
                                 RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    CALL METHOD LINK_SERVER->START_LINK_SERVER
                      IMPORTING RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

  ENDIF.                               "factory IS INITIAL.

ENDFORM.                               " INIT_FACTORY
*&---------------------------------------------------------------------*
*&      Form  CLOSE_DOC
*&---------------------------------------------------------------------*
FORM CLOSE_DOC.

  IF NOT DOCUMENT IS INITIAL.

    CALL METHOD DOCUMENT->IS_DESTROYED
                                 IMPORTING RET_VALUE = IS_CLOSED.

    IF IS_CLOSED IS INITIAL.
      CALL METHOD DOCUMENT->CLOSE_DOCUMENT
                     EXPORTING DO_SAVE = 'X'
                     IMPORTING RETCODE = RETCODE.
      CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.
    ENDIF.

    CALL METHOD DOCUMENT->RELEASE_DOCUMENT
                                 IMPORTING RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    FREE DOCUMENT.
  ENDIF.

ENDFORM.                               " CLOSE_DOC
*&---------------------------------------------------------------------*
*&      Form  CLOSE_FACTORY
*&---------------------------------------------------------------------*
FORM CLOSE_FACTORY.

  IF NOT LINK_SERVER IS INITIAL.
    CALL METHOD LINK_SERVER->STOP_LINK_SERVER
                                   IMPORTING RETCODE = RETCODE.
    FREE LINK_SERVER.
  ENDIF.
  IF NOT FACTORY IS INITIAL.
    CALL METHOD FACTORY->STOP_FACTORY IMPORTING RETCODE = RETCODE.
    FREE FACTORY.
  ENDIF.

ENDFORM.                               " CLOSE_FACTORY
*&---------------------------------------------------------------------*
*&      DEFINE  MACRO
*&---------------------------------------------------------------------*
DEFINE MACRO.
* &1 - Name of Macro
  CALL METHOD DOCUMENT->EXECUTE_MACRO
          EXPORTING MACRO_STRING = &1
          IMPORTING RETCODE = RETCODE.
  CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.
END-OF-DEFINITION.                              " MACRO
