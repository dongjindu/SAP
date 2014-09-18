*&---------------------------------------------------------------------*
*&  Include           ZITARCR
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS .

  DATA L_HANDLE TYPE SY-TABIX.
  DATA L_OFFSET LIKE ARCH_IDX-OFFSET.
  DATA L_ARKEY LIKE HEADA-ARKEY.
  DATA L_RNAME LIKE ARC_BUFFER-RNAME.

* Read archive file open
  CALL FUNCTION 'ARCHIVE_OPEN_FOR_READ'
     EXPORTING
         ARCHIVE_DOCUMENT   = '000000'
         ARCHIVE_NAME       = ' '
          OBJECT             = OBJECT
         MAINTAIN_INDEX     = ' '
     IMPORTING
          ARCHIVE_HANDLE     = L_HANDLE
*    TABLES
*         ARCHIVE_FILES      =
*         SELECTED_FILES     =
     EXCEPTIONS
          FILE_ALREADY_OPEN  = 1
          FILE_IO_ERROR      = 2
          INTERNAL_ERROR     = 3
          NO_FILES_AVAILABLE = 4
          OBJECT_NOT_FOUND   = 5
          OPEN_ERROR         = 6
          NOT_AUTHORIZED     = 7
          OTHERS             = 8.

  IF SY-SUBRC NE 0.
    LEAVE.
    LEAVE TO TRANSACTION ' '.
  ENDIF.

  DO.
*  Read data from archived file
    CALL FUNCTION 'ARCHIVE_GET_NEXT_OBJECT'
         EXPORTING
              ARCHIVE_HANDLE          = L_HANDLE
         IMPORTING
*           OBJECT_ID               =
              OBJECT_OFFSET           = L_OFFSET
              ARCHIVE_NAME            = L_ARKEY
         EXCEPTIONS
              END_OF_FILE             = 01
              OPEN_ERROR              = 03
              WRONG_ACCESS_TO_ARCHIVE = 04.

    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.

    DO.
*     Read sequencial data from archive obj.
      CALL FUNCTION 'ARCHIVE_GET_NEXT_RECORD'
        EXPORTING
          ARCHIVE_HANDLE   = L_HANDLE
        IMPORTING
          RECORD           = T_ITAB
          RECORD_STRUCTURE = L_RNAME
        EXCEPTIONS
          END_OF_OBJECT    = 01.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
*     Internal Table Filling for Deletion.
      PERFORM WRITE_PROCESS.
    ENDDO.
  ENDDO.

ENDFORM.                    " READ_PROCESS
