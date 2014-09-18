FUNCTION ZCATS_GET_LONGTEXT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(COUNTER) LIKE  CATSDBCOMM-COUNTER
*"       EXPORTING
*"             VALUE(TEXT_HEADER) TYPE  THEAD
*"       TABLES
*"              IT_LONGTEXT STRUCTURE  TLINE
*"       EXCEPTIONS
*"              TEXT_NOT_FOUND
*"----------------------------------------------------------------------

  DATA: TEXT_OBJECT LIKE THEAD-TDOBJECT VALUE 'CATS',
        TEXT_ID     LIKE THEAD-TDID     VALUE 'CATS',
        TEXT_NAME   LIKE THEAD-TDNAME,
        IT_HEADER   TYPE STANDARD TABLE OF THEAD,
        HEADER      TYPE THEAD.

  TEXT_NAME = COUNTER.

  CALL FUNCTION 'SELECT_TEXT'
       EXPORTING
            OBJECT                  = TEXT_OBJECT
            NAME                    = TEXT_NAME
            ID                      = TEXT_ID
            LANGUAGE                = '*'
       TABLES
            SELECTIONS              = IT_HEADER
       EXCEPTIONS
            WRONG_ACCESS_TO_ARCHIVE = 1
            OTHERS                  = 2.

  IF SY-SUBRC = 0 AND NOT IT_HEADER[] IS INITIAL.

    READ TABLE IT_HEADER INDEX 1 INTO HEADER.

  ELSE.

    MESSAGE E359(LR)
    RAISING TEXT_NOT_FOUND.

  ENDIF.

  TEXT_HEADER = HEADER.

  CALL FUNCTION 'READ_TEXT'
       EXPORTING
            ID                      = HEADER-TDID
            LANGUAGE                = HEADER-TDSPRAS
            NAME                    = HEADER-TDNAME
            OBJECT                  = HEADER-TDOBJECT
       TABLES
            LINES                   = IT_LONGTEXT
       EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.

  IF SY-SUBRC <> 0.

    MESSAGE E359(LR)
    RAISING TEXT_NOT_FOUND.

  ENDIF.

ENDFUNCTION.
