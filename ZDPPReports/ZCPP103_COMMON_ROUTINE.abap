*----------------------------------------------------------------------*
*   INCLUDE ZIPP001_COMMON_ROUTINE                                     *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  BDC_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_UPLOAD_DATA.
  CALL FUNCTION 'UPLOAD'
       EXPORTING
            CODEPAGE                = 'DAT'
            FILENAME                = WA_FILENAME
            FILETYPE                = WA_FILETYPE
       TABLES
            DATA_TAB                = IT_REC
       EXCEPTIONS
            CONVERSION_ERROR        = 1
            INVALID_TABLE_WIDTH     = 2
            INVALID_TYPE            = 3
            NO_BATCH                = 4
            UNKNOWN_ERROR           = 5
            GUI_REFUSE_FILETRANSFER = 6
            OTHERS                  = 7.

  IF SY-SUBRC <> 0.
    WRITE: /, ' Error Opening File: ', WA_FILENAME,
          /, ' Return Code: ', SY-SUBRC.
    EXIT.
  ENDIF.
ENDFORM.                    " BDC_UPLOAD_DATA

*&---------------------------------------------------------------------*
*&      Form  BDC_OPEN_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT              = SY-MANDT
            GROUP               = WA_BDCGROUP
            KEEP                = 'X'
            USER                = SY-UNAME
       EXCEPTIONS
            CLIENT_INVALID      = 1
            DESTINATION_INVALID = 2
            GROUP_INVALID       = 3
            GROUP_IS_LOCKED     = 4
            HOLDDATE_INVALID    = 5
            INTERNAL_ERROR      = 6
            QUEUE_ERROR         = 7
            RUNNING             = 8
            SYSTEM_LOCK_ERROR   = 9
            USER_INVALID        = 10
            OTHERS              = 11.

  IF SY-SUBRC <> 0.
    WRITE: /, ' Error BDC Opening Group: ', SY-UZEIT,
           /, ' Return Code: ', SY-SUBRC.
    EXIT.
  ENDIF.
ENDFORM.                    " BDC_OPEN_GROUP

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0183   text
*      -->P_0184   text
*      -->P_0185   text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO_PROCESSING USING    DY_BEGIN  PG_NAME   SC_NO.
  IF DY_BEGIN = 'X'.
    CLEAR IT_BDCDATA.
    MOVE  PG_NAME  TO IT_BDCDATA-PROGRAM.
    MOVE  SC_NO    TO IT_BDCDATA-DYNPRO.
    MOVE  'X'      TO IT_BDCDATA-DYNBEGIN.
    APPEND IT_BDCDATA.
  ELSE.
    CLEAR IT_BDCDATA.
    MOVE  PG_NAME  TO IT_BDCDATA-FNAM.
    MOVE  SC_NO    TO IT_BDCDATA-FVAL.
    APPEND IT_BDCDATA.
  ENDIF.
ENDFORM.                    " BDC_DYNPRO_PROCESSING.

*&---------------------------------------------------------------------*
*&      Form  BDC_INSERT_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_INSERT_TRANSACTION.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE            = P_TCODE
       TABLES
            DYNPROTAB        = IT_BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR   = 1
            NOT_OPEN         = 2
            QUEUE_ERROR      = 3
            TCODE_INVALID    = 4
            PRINTING_INVALID = 5
            POSTING_INVALID  = 6
            OTHERS           = 7.

  IF SY-SUBRC <> 0.
    WRITE: /, ' Error BDC Insert: ', SY-UZEIT,
           /, ' Return Code: ', SY-SUBRC.
    LEAVE.
  ENDIF.
ENDFORM.                    " BDC_INSERT_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  BDC_CLOSE_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_CLOSE_GROUP.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN    = 1
            QUEUE_ERROR = 2
            OTHERS      = 3.

  IF SY-SUBRC <> 0.
    WRITE: /, ' Error BDC Close: ', SY-UZEIT,
           /, ' Return Code: ', SY-SUBRC.
    EXIT.
  ENDIF.
ENDFORM.                    " BDC_CLOSE_GROUP

*&---------------------------------------------------------------------*
*&      Form  CREATE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TMP_TEXT  text
*----------------------------------------------------------------------*
FORM CREATE_MESSAGE  USING P_TEXT  PID  PNR  P01  P02  P03  P04 .
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = PID
            MSGNR               = PNR
            MSGV1               = P01
            MSGV2               = P02
            MSGV3               = P03
            MSGV4               = P04
       IMPORTING
            MESSAGE_TEXT_OUTPUT = P_TEXT.
ENDFORM.                    " CREATE_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TMP_TEXT  text
*----------------------------------------------------------------------*
FORM DISPLAY_MESSAGE  USING P_TEXT  PTYPE  PTOTAL PPROCESS .
  CONCATENATE 'Processed : ' PPROCESS '/' PTOTAL P_TEXT  INTO P_TEXT
                                          SEPARATED BY SPACE         .

  CASE PTYPE .
    WHEN 'I' .
      MESSAGE I001 WITH P_TEXT       .
    WHEN 'W' .
      MESSAGE W001 WITH P_TEXT       .
    WHEN 'S' .
      MESSAGE S001 WITH P_TEXT       .
    WHEN 'E' .
      MESSAGE E001 WITH P_TEXT       .
    WHEN 'A' .
      MESSAGE A001 WITH P_TEXT       .
    WHEN 'X' .
      MESSAGE X001 WITH P_TEXT       .
  ENDCASE.
ENDFORM.                    " CREATE_MESSAGE
