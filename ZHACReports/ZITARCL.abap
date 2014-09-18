*&---------------------------------------------------------------------*
*&  Include           ZITARCL
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  reloading_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RELOADING_PROCESS.
  DATA: L_REF_DATA TYPE REF TO DATA.
  FIELD-SYMBOLS: <FS_RECORD> TYPE ANY.      "Unicode
* open an existing archive file for read
  CALL FUNCTION 'ARCHIVE_OPEN_FOR_MOVE'
    EXPORTING
      OBJECT               = OBJECT
      TEST_MODE            = TESTRUN
    IMPORTING
      ARCHIVE_READ_HANDLE  = READ
      ARCHIVE_WRITE_HANDLE = WRITE.
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
    EXCEPTIONS
      OBJECT_NOT_FOUND            = 01.

* loop to get the next object from the archiv files
  OBJCNT = 0.
  DO.
    OBJCNT = OBJCNT + 1.
    CALL FUNCTION 'ARCHIVE_GET_NEXT_OBJECT'
      EXPORTING
        ARCHIVE_HANDLE = READ
      EXCEPTIONS
        END_OF_FILE    = 01.
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.

    DO.
      CALL FUNCTION 'ARCHIVE_GET_NEXT_RECORD'    "builds up index
           EXPORTING
                ARCHIVE_HANDLE          = READ
           IMPORTING
*                RECORD                  = ARC_BUFFER-SEGMENT
                RECORD_REF               = L_REF_DATA               "Unicode
                RECORD_STRUCTURE        = ARC_BUFFER-RNAME
           EXCEPTIONS
                END_OF_OBJECT           = 01
                INTERNAL_ERROR          = 02
                WRONG_ACCESS_TO_ARCHIVE = 03.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.

*     Internal Table Filling for Deletion.
*      MOVE ARC_BUFFER-SEGMENT TO T_ITAB.
      ASSIGN L_REF_DATA->* TO <FS_RECORD>.                 "Unicode
      MOVE <FS_RECORD> TO T_ITAB.                          "Unicode
      APPEND T_ITAB.
      ADD 1 TO DATA_CNT.
    ENDDO.

* delete records from the object
    IF ( TESTRUN = SPACE ) AND ( OBJCNT > COMMCNT ).
      PERFORM INSERT_FROM_TABLE.
      OBJCNT = 0.
    ENDIF.
  ENDDO.

* for the last objects
  IF TESTRUN = SPACE.
    PERFORM INSERT_FROM_TABLE.
  ENDIF.

* write result
  PERFORM LIST_COUNTER.

* close the archiv file
  CALL FUNCTION 'ARCHIVE_CLOSE_FILE'
    EXPORTING
      ARCHIVE_HANDLE = READ.

ENDFORM.                    " reloading_process

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
