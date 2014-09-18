*&---------------------------------------------------------------------*
*&  Include           ZITARCW
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  archive_process
*&---------------------------------------------------------------------*
FORM ARCHIVE_PROCESS.

* open a new archiv for write
  CALL FUNCTION 'ARCHIVE_OPEN_FOR_WRITE'
    EXPORTING
      CALL_DELETE_JOB_IN_TEST_MODE = TESTRUN
      COMMENTS                     = COMMENT
      CREATE_ARCHIVE_FILE          = CREATE
      OBJECT                       = OBJECT
    IMPORTING
      ARCHIVE_HANDLE               = HANDLE.

* Open Cursor for DB read
  PERFORM OPEN_CURSOR_FOR_DB.

  DO.
    REFRESH T_ITAB.
    FETCH NEXT CURSOR G_CURSOR
        INTO TABLE T_ITAB
        PACKAGE SIZE G_PACKAGE.
    IF NOT SY-SUBRC IS INITIAL.
      EXIT.
    ENDIF.

    LOOP AT T_ITAB.
      PERFORM MAKE_ARCHIVE_OBJECT_ID.     " For index create

      CALL FUNCTION 'ARCHIVE_NEW_OBJECT'
        EXPORTING
          ARCHIVE_HANDLE = HANDLE
          OBJECT_ID      = DATA_OBJECT_ID. " Optional: ADK index

* write the first record to the object
      CALL FUNCTION 'ARCHIVE_PUT_RECORD'
        EXPORTING
          ARCHIVE_HANDLE   = HANDLE
          RECORD_STRUCTURE = ARC_STRUCT
          RECORD_FLAGS     = ' '
          RECORD           = T_ITAB.

* write the object to the archiv file
      CALL FUNCTION 'ARCHIVE_SAVE_OBJECT'
        EXPORTING
          ARCHIVE_HANDLE = HANDLE.
    ENDLOOP.
  ENDDO.

  CLOSE CURSOR G_CURSOR.

* write archive statistic
  CALL FUNCTION 'ARCHIVE_WRITE_STATISTICS'
    EXPORTING
      ARCHIVE_HANDLE = HANDLE.

* close the archiv file
  CALL FUNCTION 'ARCHIVE_CLOSE_FILE'
    EXPORTING
      ARCHIVE_HANDLE = HANDLE.
ENDFORM.                    " archive_process
