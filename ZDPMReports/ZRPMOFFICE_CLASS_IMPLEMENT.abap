*----------------------------------------------------------------------*
*   INCLUDE ZRPMOFFICE_CLASS_DEFINITION                                *
*----------------------------------------------------------------------*

CLASS C_OI_FACTORY_CREATOR IMPLEMENTATION.
  METHOD GET_DOCUMENT_FACTORY.
    DATA: OLEFACTORYOBJ TYPE REF TO C_OI_OLE_DOCUMENT_FACTORY.
    DATA: HAS_ACTIVEX,HAS_JAVABEAN.

    CALL FUNCTION 'GUI_HAS_ACTIVEX'
         IMPORTING
              RETURN = HAS_ACTIVEX.

    CALL FUNCTION 'GUI_HAS_JAVABEANS'
         IMPORTING
              RETURN = HAS_JAVABEAN.

    IF ( NOT HAS_ACTIVEX IS INITIAL ) OR
        ( NOT HAS_JAVABEAN IS INITIAL ).
      CREATE OBJECT OLEFACTORYOBJ.
      FACTORY = OLEFACTORYOBJ.
      RETCODE = C_OI_ERRORS=>RET_OK.
    ELSE.
      RETCODE = C_OI_ERRORS=>RET_ERROR.
    ENDIF.

    CALL METHOD C_OI_ERRORS=>CREATE_ERROR_FOR_RETCODE
                             EXPORTING RETCODE = RETCODE
                                       NO_FLUSH = ' '
                             IMPORTING ERROR = ERROR.
  ENDMETHOD.
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS c_oi_ole_control_creator IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS C_OI_OLE_CONTROL_CREATOR IMPLEMENTATION.
  METHOD GET_OLE_CONTAINER_CONTROL.

    DATA: L_CONTROL TYPE REF TO I_OI_CONTAINER_CONTROL.

    CALL METHOD C_OI_CONTAINER_CONTROL_CREATOR=>GET_CONTAINER_CONTROL
               IMPORTING CONTROL = L_CONTROL
                         RETCODE = RETCODE
                         ERROR = ERROR.
    CONTROL ?= L_CONTROL.

  ENDMETHOD.
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS c_oi_ole_document_factory IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS C_OI_OLE_DOCUMENT_FACTORY IMPLEMENTATION.
  METHOD I_OI_DOCUMENT_FACTORY~START_FACTORY.

    IF CONTAINER_CONTROL IS INITIAL.
      CALL METHOD C_OI_OLE_CONTROL_CREATOR=>GET_OLE_CONTAINER_CONTROL
         IMPORTING CONTROL = CONTAINER_CONTROL
                   RETCODE = RETCODE.
      IF RETCODE EQ C_OI_ERRORS=>RET_OK.

        CREATE OBJECT CONTAINER
            EXPORTING CONTAINER_NAME = 'CONTAINER'.

        CALL METHOD CONTAINER_CONTROL->INIT_CONTROL
           EXPORTING R3_APPLICATION_NAME = R3_APPLICATION_NAME
*                     INPLACE_MODE    = 1
                     INPLACE_ENABLED = 'X'
                     INPLACE_SCROLL_DOCUMENTS = 'X'
                     REP_ID          = REP_ID
*                     SHELL_STYLE = CL_GUI_CONTROL=>WS_CHILD
*                     DYNPRO_NR = DYNPRO_NR
                     PARENT = CONTAINER
                              "CL_GUI_CONTAINER=>DEFAULT_SCREEN
                     REGISTER_ON_CLOSE_EVENT = REGISTER_ON_CLOSE_EVENT
                     REGISTER_ON_CUSTOM_EVENT =
                                              REGISTER_ON_CUSTOM_EVENT
                     NO_FLUSH = NO_FLUSH
           IMPORTING RETCODE = RETCODE
                     ERROR = ERROR.
      ELSE.
        CALL METHOD C_OI_ERRORS=>CREATE_ERROR_FOR_RETCODE
                                 EXPORTING RETCODE = RETCODE
                                           NO_FLUSH = NO_FLUSH
                                 IMPORTING ERROR = ERROR.
      ENDIF.
    ELSE. " already created
      RETCODE = C_OI_ERRORS=>RET_OK.
      CALL METHOD C_OI_ERRORS=>CREATE_ERROR_FOR_RETCODE
                                 EXPORTING RETCODE = RETCODE
                                           NO_FLUSH = NO_FLUSH
                                 IMPORTING ERROR = ERROR.
    ENDIF.
  ENDMETHOD.

  METHOD I_OI_DOCUMENT_FACTORY~STOP_FACTORY.

    IF NOT CONTAINER_CONTROL IS INITIAL.
      CALL METHOD CONTAINER_CONTROL->DESTROY_CONTROL
           EXPORTING NO_FLUSH = NO_FLUSH
           IMPORTING RETCODE = RETCODE
                     ERROR = ERROR.
    ELSE. " already destroyed
      RETCODE = C_OI_ERRORS=>RET_OK.
      CALL METHOD C_OI_ERRORS=>CREATE_ERROR_FOR_RETCODE
                                 EXPORTING RETCODE = RETCODE
                                           NO_FLUSH = NO_FLUSH
                                 IMPORTING ERROR = ERROR.
    ENDIF.
  ENDMETHOD.

  METHOD I_OI_DOCUMENT_FACTORY~LINK_FACTORY.
    IF NOT CONTAINER_CONTROL IS INITIAL.
      CALL METHOD CONTAINER_CONTROL->LINK_CONTROL
                    EXPORTING REP_ID = REP_ID
                              DYNPRO_NR = DYNPRO_NR
                              NO_FLUSH = NO_FLUSH
                    IMPORTING RETCODE = RETCODE
                              ERROR = ERROR.
    ELSE.
      RETCODE = C_OI_ERRORS=>RET_OBJECT_NOT_INITIALIZED.
      CALL METHOD C_OI_ERRORS=>CREATE_ERROR_FOR_RETCODE
                                  EXPORTING RETCODE = RETCODE
                                            NO_FLUSH = NO_FLUSH
                                  IMPORTING ERROR = ERROR.
    ENDIF.
  ENDMETHOD.

  METHOD I_OI_DOCUMENT_FACTORY~GET_DOCUMENT_PROXY.

    IF NOT CONTAINER_CONTROL IS INITIAL.
      CALL METHOD CONTAINER_CONTROL->GET_DOCUMENT_PROXY
                    EXPORTING DOCUMENT_TYPE = DOCUMENT_TYPE
                              DOCUMENT_FORMAT = DOCUMENT_FORMAT
                              REGISTER_CONTAINER = REGISTER_CONTAINER
                              NO_FLUSH = NO_FLUSH
                    IMPORTING DOCUMENT_PROXY = DOCUMENT_PROXY
                              RETCODE = RETCODE
                              ERROR = ERROR.
    ELSE.
      RETCODE = C_OI_ERRORS=>RET_OBJECT_NOT_INITIALIZED.
      CALL METHOD C_OI_ERRORS=>CREATE_ERROR_FOR_RETCODE
                                  EXPORTING RETCODE = RETCODE
                                            NO_FLUSH = NO_FLUSH
                                  IMPORTING ERROR = ERROR.
    ENDIF.
  ENDMETHOD.

  METHOD I_OI_DOCUMENT_FACTORY~GET_LINK_SERVER.
    IF NOT CONTAINER_CONTROL IS INITIAL.
      CALL METHOD CONTAINER_CONTROL->GET_LINK_SERVER
                    EXPORTING SERVER_TYPE = SERVER_TYPE
                              NO_FLUSH = NO_FLUSH
                    IMPORTING LINK_SERVER = LINK_SERVER
                              RETCODE = RETCODE
                              ERROR = ERROR.
    ELSE.
      RETCODE = C_OI_ERRORS=>RET_OBJECT_NOT_INITIALIZED.
      CALL METHOD C_OI_ERRORS=>CREATE_ERROR_FOR_RETCODE
                                  EXPORTING RETCODE = RETCODE
                                            NO_FLUSH = NO_FLUSH
                                  IMPORTING ERROR = ERROR.
    ENDIF.
  ENDMETHOD.

  METHOD I_OI_DOCUMENT_FACTORY~GET_TABLE_COLLECTION.
    IF NOT CONTAINER_CONTROL IS INITIAL.
      CALL METHOD CONTAINER_CONTROL->GET_TABLE_COLLECTION
                    EXPORTING NO_FLUSH = NO_FLUSH
                    IMPORTING TABLE_COLLECTION = TABLE_COLLECTION
                              RETCODE = RETCODE
                              ERROR = ERROR.
    ELSE.
      RETCODE = C_OI_ERRORS=>RET_OBJECT_NOT_INITIALIZED.
      CALL METHOD C_OI_ERRORS=>CREATE_ERROR_FOR_RETCODE
                                  EXPORTING RETCODE = RETCODE
                                            NO_FLUSH = NO_FLUSH
                                  IMPORTING ERROR = ERROR.
    ENDIF.
  ENDMETHOD.

  METHOD I_OI_DOCUMENT_FACTORY~GET_REGISTERED_DOC_TYPES.
    IF NOT CONTAINER_CONTROL IS INITIAL.
      CALL METHOD CONTAINER_CONTROL->GET_REGISTERED_DOC_TYPES
                    EXPORTING INTERFACE_TYPE = INTERFACE_TYPE
                              NO_FLUSH = NO_FLUSH
                    IMPORTING DESCR_LIST = DESCR_LIST
                              RETCODE = RETCODE
                              ERROR = ERROR.
    ELSE.
      RETCODE = C_OI_ERRORS=>RET_OBJECT_NOT_INITIALIZED.
      CALL METHOD C_OI_ERRORS=>CREATE_ERROR_FOR_RETCODE
                                  EXPORTING RETCODE = RETCODE
                                            NO_FLUSH = NO_FLUSH
                                  IMPORTING ERROR = ERROR.
    ENDIF.
  ENDMETHOD.

  METHOD I_OI_DOCUMENT_FACTORY~CHECK_REGISTERED_DOC_TYPES.

    IF NOT CONTAINER_CONTROL IS INITIAL.
      CALL METHOD CONTAINER_CONTROL->CHECK_REGISTERED_DOC_TYPES
                    EXPORTING NO_FLUSH = NO_FLUSH
                    IMPORTING ERROR = ERROR
                    CHANGING DESCR_LIST = DESCR_LIST
                             RETCODE = RETCODE.
    ELSE.
      RETCODE = C_OI_ERRORS=>RET_OBJECT_NOT_INITIALIZED.
      CALL METHOD C_OI_ERRORS=>CREATE_ERROR_FOR_RETCODE
                                  EXPORTING RETCODE = RETCODE
                                            NO_FLUSH = NO_FLUSH
                                  IMPORTING ERROR = ERROR.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
