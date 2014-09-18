FUNCTION ZMRER_PARALLEL_PROCESSING.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(JOB) LIKE  TBTCJOB-JOBNAME
*"  TABLES
*"      IT_SUBMIT_PASS STRUCTURE  RSPARAMS
*"----------------------------------------------------------------------
  data : no type TBTCJOB-JOBCOUNT.
*  data:     job like TBTCJOB-JOBNAME value 'TESTJOB'.

  DATA: BEGIN OF PRINT_PARAMETERS.
          INCLUDE STRUCTURE PRI_PARAMS.
  DATA: END OF PRINT_PARAMETERS.
  DATA: PP_VALID TYPE C.
* Default print parameters
CALL FUNCTION 'GET_PRINT_PARAMETERS'
 EXPORTING
*   ARCHIVE_ID                   = C_CHAR_UNKNOWN
*   ARCHIVE_INFO                 = C_CHAR_UNKNOWN
*   ARCHIVE_MODE                 = C_CHAR_UNKNOWN
*   ARCHIVE_TEXT                 = C_CHAR_UNKNOWN
*   AR_OBJECT                    = C_CHAR_UNKNOWN
*   ARCHIVE_REPORT               = C_CHAR_UNKNOWN
*   AUTHORITY                    = C_CHAR_UNKNOWN
*   COPIES                       = C_NUM3_UNKNOWN
*   COVER_PAGE                   = C_CHAR_UNKNOWN
*   DATA_SET                     = C_CHAR_UNKNOWN
*   DEPARTMENT                   = C_CHAR_UNKNOWN
*   DESTINATION                  = C_CHAR_UNKNOWN
*   EXPIRATION                   = C_NUM1_UNKNOWN
*   IMMEDIATELY                  = C_CHAR_UNKNOWN
*   IN_ARCHIVE_PARAMETERS        = ' '
*   IN_PARAMETERS                = ' '
*   LAYOUT                       = C_CHAR_UNKNOWN
   LINE_COUNT                   = 65
   LINE_SIZE                    = 132
*   LIST_NAME                    = C_CHAR_UNKNOWN
*   LIST_TEXT                    = C_CHAR_UNKNOWN
   MODE                         = 'BATCH'
*   NEW_LIST_ID                  = C_CHAR_UNKNOWN
   NO_DIALOG                    = 'X'
*   RECEIVER                     = C_CHAR_UNKNOWN
*   RELEASE                      = C_CHAR_UNKNOWN
   REPORT                       = 'RMMR1MRR'
*   SAP_COVER_PAGE               = C_CHAR_UNKNOWN
*   HOST_COVER_PAGE              = C_CHAR_UNKNOWN
*   PRIORITY                     = C_NUM1_UNKNOWN
*   SAP_OBJECT                   = C_CHAR_UNKNOWN
*   TYPE                         = C_CHAR_UNKNOWN


*   USER                         = SY-UNAME
 IMPORTING
*   OUT_ARCHIVE_PARAMETERS       =
   OUT_PARAMETERS               = print_parameters
   VALID                        = PP_valid
 EXCEPTIONS
   ARCHIVE_INFO_NOT_FOUND       = 1
   INVALID_PRINT_PARAMS         = 2
   INVALID_ARCHIVE_PARAMS       = 3
   OTHERS                       = 4.

if SY-SUBRC <> 0.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
* open the job.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
*   DELANFREP              = ' '
*   JOBGROUP               = ' '
      JOBNAME                = job
*   SDLSTRTDT              = NO_DATE
*   SDLSTRTTM              = NO_TIME
   IMPORTING
     JOBCOUNT               = no
   EXCEPTIONS
     CANT_CREATE_JOB        = 1
     INVALID_JOB_DATA       = 2
     JOBNAME_MISSING        = 3
     OTHERS                 = 4.

  IF SY-SUBRC <> 0.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.


* submit program with the current passed packe

 SUBMIT RMMR1MRR
        TO SAP-SPOOL
        SPOOL PARAMETERS PRINT_PARAMETERS
        WITHOUT SPOOL DYNPRO
        AND RETURN
        USER SY-UNAME
        VIA JOB JOB NUMBER NO
        WITH SELECTION-TABLE IT_SUBMIT_PASS.

* close job

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
*    AT_OPMODE                         = ' '
*    AT_OPMODE_PERIODIC                = ' '
*    CALENDAR_ID                       = ' '
*    EVENT_ID                          = ' '


*    EVENT_PARAM                       = ' '
*    EVENT_PERIODIC                    = ' '
      JOBCOUNT                          = no
      JOBNAME                           = job
*    LASTSTRTDT                        = NO_DATE
*    LASTSTRTTM                        = NO_TIME
*    PRDDAYS                           = 0
*    PRDHOURS                          = 0
*    PRDMINS                           = 0
*    PRDMONTHS                         = 0
*    PRDWEEKS                          = 0
*    PREDJOB_CHECKSTAT                 = ' '
*    PRED_JOBCOUNT                     = ' '
*    PRED_JOBNAME                      = ' '
*    SDLSTRTDT                         = NO_DATE
*    SDLSTRTTM                         = NO_TIME
*    STARTDATE_RESTRICTION             = BTC_PROC
     STRTIMMED                         = 'X'
*    TARGETSYSTEM                      = ' '
*    START_ON_WORKDAY_NOT_BEFORE       = SY-DATUM
*    START_ON_WORKDAY_NR               = 0
*    WORKDAY_COUNT_DIRECTION           = 0
*    RECIPIENT_OBJ                     =
*    TARGETSERVER                      = ' '
*    DONT_RELEASE                      = ' '
*  IMPORTING
*    JOB_WAS_RELEASED                  =
    EXCEPTIONS
       CANT_START_IMMEDIATE              = 1
       INVALID_STARTDATE                 = 2
       JOBNAME_MISSING                   = 3
       JOB_CLOSE_FAILED                  = 4
       JOB_NOSTEPS                       = 5
       JOB_NOTEX                         = 6
       LOCK_FAILED                       = 7
       OTHERS                            = 8.

    IF SY-SUBRC <> 0.
       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

ENDFUNCTION.
