*&---------------------------------------------------------------------*
*&  Include           ZITARCD
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  delete_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_PROCESS.
  DATA: L_REF_DATA TYPE REF TO DATA.
  FIELD-SYMBOLS: <FS_RECORD> TYPE ANY.      "Unicode

* open an existing archive file for read
  CALL FUNCTION 'ARCHIVE_OPEN_FOR_DELETE'
    EXPORTING
      OBJECT             = OBJECT
      TEST_MODE          = TESTRUN
    IMPORTING
      ARCHIVE_HANDLE     = HANDLE
    EXCEPTIONS
      FILE_ALREADY_OPEN  = 01
      FILE_IO_ERROR      = 02
      NO_FILES_AVAILABLE = 04
      OBJECT_NOT_FOUND   = 05
      OPEN_ERROR         = 06.
  IF SY-SUBRC NE 0.
    LEAVE.
    LEAVE TO TRANSACTION ' '.
  ENDIF.

* get customizing
  CALL FUNCTION 'ARCHIVE_GET_CUSTOMIZING_DATA'
    EXPORTING
      OBJECT                      = OBJECT
    IMPORTING
      COMMIT_COUNT_FOR_DELETE_PRG = COMMCNT
      MAINTAIN_INDEX              = INDEX
    EXCEPTIONS
      OBJECT_NOT_FOUND            = 01.

* loop to get the next object from the archiv files
  OBJCNT = 0.
  DO.
    OBJCNT = OBJCNT + 1.
    CALL FUNCTION 'ARCHIVE_GET_NEXT_OBJECT'
      EXPORTING
        ARCHIVE_HANDLE          = HANDLE
      IMPORTING
*       OBJECT_ID               =
        OBJECT_OFFSET           = OFFSET
        ARCHIVE_NAME            = ARKEY
      EXCEPTIONS
        END_OF_FILE             = 01
        OPEN_ERROR              = 03
        WRONG_ACCESS_TO_ARCHIVE = 04.
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.

    DO.
      CALL FUNCTION 'ARCHIVE_GET_NEXT_RECORD'    "builds up index
           EXPORTING
                ARCHIVE_HANDLE          = HANDLE
           IMPORTING
*                RECORD                  = ARC_BUFFER-SEGMENT
                RECORD_REF               = L_REF_DATA               "Unicode
                RECORD_STRUCTURE        = ARC_BUFFER-RNAME
           EXCEPTIONS
                END_OF_OBJECT           = 01.

      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
**     Internal Table Filling for Deletion.
*    MOVE ARC_BUFFER-SEGMENT TO T_ITAB.
*    T_ITAB = ARC_BUFFER-SEGMENT.
      ASSIGN L_REF_DATA->* TO <FS_RECORD>.                 "Unicode
      MOVE <FS_RECORD> TO T_ITAB.                          "Unicode
      APPEND T_ITAB.
      ADD 1 TO DATA_CNT.
    ENDDO.

* delete records from the object
    IF ( TESTRUN = SPACE ) AND ( OBJCNT > COMMCNT ).
      PERFORM DELETE_FROM_TABLE.
      OBJCNT = 0.
    ENDIF.
  ENDDO.

* for the last objects
  IF TESTRUN = SPACE.
    PERFORM DELETE_FROM_TABLE.
  ENDIF.

* write result
  PERFORM LIST_COUNTER.

* close the archiv file
  CALL FUNCTION 'ARCHIVE_CLOSE_FILE'
    EXPORTING
      ARCHIVE_HANDLE = HANDLE.

ENDFORM.                    " delete_process

*&---------------------------------------------------------------------*
*&      Form  list_counter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIST_COUNTER.

  DATA: SW_COLOR(1) TYPE C.

  WRITE: / 'Statistics of archived objects'.
  FORMAT COLOR COL_KEY INTENSIFIED.
  WRITE: /3 'Table', 15 'Number', 25 'Description'.
  FORMAT COLOR COL_KEY INTENSIFIED OFF.
  IF SW_COLOR EQ SPACE.
    FORMAT COLOR COL_NORMAL INTENSIFIED.
    SW_COLOR = 'X'.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    SW_COLOR = ' '.
  ENDIF.

  WRITE: /3 OBJECT, 15 DATA_CNT.

ENDFORM.                    " list_counter
