REPORT zresult_sap .
DATA : it_sapwlpfnrm LIKE sapwlpfnrm OCCURS 0 WITH HEADER LINE .
DATA : BEGIN OF it_data OCCURS 0,
        recno LIKE  sapwlpfnrm-recno,
        date LIKE sapwlpfnrm-date,
        startdate LIKE sapwlpfnrm-startdate,
        starttime LIKE sapwlpfnrm-starttime,
        enddate   LIKE sapwlpfnrm-enddate,
        endtime LIKE sapwlpfnrm-endtime,
        account LIKE sapwlpfnrm-account,
        terminalid LIKE sapwlpfnrm-terminalid,
        tcode LIKE sapwlpfnrm-tcode,
        ttext  LIKE tstct-ttext,
        cua_prog LIKE  sapwlpfnrm-cua_prog,
        cua_func LIKE  sapwlpfnrm-cua_func,
       END OF it_data.

REFRESH it_data.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS : s_date  FOR sy-datum DEFAULT sy-datum,
                 s_time  FOR sy-uzeit DEFAULT sy-uzeit,
                 s_uname FOR sy-uname DEFAULT sy-uname
                            NO INTERVALS NO-EXTENSION.

SELECTION-SCREEN END OF BLOCK b1.

CALL FUNCTION 'SAPWL_STATREC_READ_FILE'
 EXPORTING
*  NO_OF_RECORDS                     = -1
   read_client                       = sy-mandt
   read_end_date                     = s_date-high
   read_end_time                     = s_time-high
   read_exclude_username             = ' '
*   READ_CONTINUE_RECORDNO            = -1
   read_start_date                   = s_date-low
   read_start_time                   = s_time-low
   read_username                     = s_uname-low
   read_workprocess                  = 'FFFF'
   read_forward                      = 'X'
*   STATISTIC_FILE                    =
*   NO_BUFFER_FLUSH                   = ' '
* IMPORTING
*   FILE_ERROR                        =
*   RECORDS_READ                      =
*   EOF_REACHED                       =
*   CONTINUE_RECORDNO_FORWARD         =
*   CONTINUE_RECORDNO_BACKWARD        =
*   PROBLEMS                          =
 TABLES
   v2_normal_records                 = it_sapwlpfnrm
*   V2_BTC_STEP_RECORDS               =
*   V2_TABLE_RECORDS                  =
*   V2_RFC_CLIENT_RECORDS             =
*   V2_RFC_SERVER_RECORDS             =
*   V2_RFC_CLIENT_DEST_RECORDS        =
*   V2_RFC_SERVER_DEST_RECORDS        =
*   V2_SPOOL_PRINT_RECORDS            =
*   V2_SPOOL_ACTIVITY_RECORDS         =
*   V2_RFC_TIME_INT_RECORDS           =
*   NORM_SUBRECORD_INDEX              =
*   DB_PROCEDURE_RECORDS              =
*   ADM_MESSAGE_RECORDS               =
 EXCEPTIONS
   wrong_parameter_combination       = 1
   OTHERS                            = 2
          .
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ELSE.

  LOOP AT it_sapwlpfnrm.
    IF  ( it_sapwlpfnrm-cua_func NE it_sapwlpfnrm-tcode ) OR
        ( it_sapwlpfnrm-tcode EQ 'ZSAP' ) OR
        ( it_sapwlpfnrm-tcode EQ space ).
      DELETE it_sapwlpfnrm INDEX sy-tabix.
    ELSE.

      MOVE-CORRESPONDING it_sapwlpfnrm TO it_data.
      SELECT SINGLE ttext INTO it_data-ttext
        FROM tstct
         WHERE sprsl EQ 'E'
           AND tcode EQ it_sapwlpfnrm-tcode.
      APPEND it_data.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
   EXPORTING
*   BIN_FILESIZE                  = ' '
*   CODEPAGE                      = ' '
*   FILENAME                      = ' '
    filetype                      = 'DAT'
*   ITEM                          = ' '
*   MODE                          = ' '
*   WK1_N_FORMAT                  = ' '
*   WK1_N_SIZE                    = ' '
*   WK1_T_FORMAT                  = ' '
*   WK1_T_SIZE                    = ' '
*   FILEMASK_MASK                 = ' '
*   FILEMASK_TEXT                 = ' '
*   FILETYPE_NO_CHANGE            = ' '
*   FILEMASK_ALL                  = ' '
*   FILETYPE_NO_SHOW              = ' '
*   SILENT                        = 'S'
*   COL_SELECT                    = ' '
*   COL_SELECTMASK                = ' '
*   NO_AUTH_CHECK                 = ' '
* IMPORTING
*   ACT_FILENAME                  =
*   ACT_FILETYPE                  =
*   FILESIZE                      =
*   CANCEL                        =
    TABLES
      data_tab                      =  it_data
*   FIELDNAMES                    =
* EXCEPTIONS
*   INVALID_FILESIZE              = 1
*   INVALID_TABLE_WIDTH           = 2
*   INVALID_TYPE                  = 3
*   NO_BATCH                      = 4
*   UNKNOWN_ERROR                 = 5
*   GUI_REFUSE_FILETRANSFER       = 6
*   CUSTOMER_ERROR                = 7
*   OTHERS                        = 8
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDIF.
