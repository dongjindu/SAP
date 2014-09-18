************************************************************************
* Program Name      : ZAPP703C_WORKORDER_MAINT_SHARE
* Author            : Bobby
* Creation Date     : 2003.09.16.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902220
* Addl Documentation:
* Description       : Interface Work Order from Legacy System
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 11/29/2004 Yongping     UD1K913223   change code for no update when
*                                      interface table is empty to avoid
*                                      sequence date missing in table
*                                      ZTPP_COMMON_VALS
************************************************************************
REPORT  ZAPP703C_WORKORDER_MAINT   LINE-SIZE  700
                                   MESSAGE-ID ZMPP.

TABLES: ZTPP_PP_LOG_HEAD,       " Table of the Interface Log(Header)
        ZTPP_PP_LOG_DETA,       " Table of the Interface Log(Detail)
        ZTPP_COMMON_VALS,       " Table of the last Working Information
        ZTPP_WOSUM    ,         " Table of the WorkOrder Summary
        ZTPP_KSBOHMM  .

DATA: BEGIN OF IT_DATA       OCCURS 0.
        INCLUDE STRUCTURE    ZTPP_KSBOHMM .
DATA:   P_FLAG               TYPE C,
        WO                   TYPE C,
        H_C                  TYPE C,
        EXIST                TYPE C,
        MATERIAL             LIKE MARA-MATNR,
      END OF IT_DATA.

DATA: IT_MSG                 LIKE TABLE OF BDCMSGCOLL  WITH HEADER LINE,
      IT_BDCDATA             LIKE TABLE OF BDCDATA     WITH HEADER LINE,
      IT_ERROR               LIKE TABLE OF IT_DATA     WITH HEADER LINE,
      IT_SUM                 LIKE TABLE OF ZTPP_WOSUM  WITH HEADER LINE,
      WA_MODE                TYPE C VALUE 'N',
      WA_DATE                LIKE SY-DATUM   ,
      WA_DATE2               LIKE SY-DATUM   ,
      WA_CHECK(4)            TYPE C       ,
      WA_CNT                 TYPE I       ,
      WA_MONTH               LIKE ZTPP_WOSUM-WO_SER,
      WA_HDCNT               TYPE I       ,
      WA_CLCNT               TYPE I       ,
      WA_FLG                 TYPE C       ,
      WA_ERROR               TYPE C       ,
      WA_NUMBER              LIKE ZTPP_PP_LOG_HEAD-LOGKEY,
      WA_DATA                LIKE IT_DATA ,
      WA_SEQ                 LIKE ZTPP_PP_LOG_DETA-SEQUENCE,
      WA_WKDATE              LIKE SY-DATUM,
      WA_TABIX               LIKE SY-TABIX.

DATA: GLOBAL_JOB LIKE TBTCJOB OCCURS 0 WITH HEADER LINE.
DATA: IT_JOBLIST LIKE TBTCJOB OCCURS 0 WITH HEADER LINE.
DATA: GLOBAL_START_DATE LIKE TBTCSTRT OCCURS 0 WITH HEADER LINE.
DATA: GLOBAL_STEP_TBL LIKE TBTCSTEP OCCURS 0 WITH HEADER LINE.

DATA: JOBC LIKE TBTCJOB-JOBCOUNT ,
      JOBN LIKE  TBTCJOB-JOBNAME ,
      IMMEDIATE LIKE BTCH0000-CHAR1 VALUE 'X'.
DATA: WA_COUNT(4) TYPE N,
      C_PROG LIKE SY-REPID.

DATA: WA_EMPTY  TYPE C.                            "  UD1K913223

DATA: BEGIN OF IT_COL_ERROR OCCURS 0,
      WO_SER LIKE ZTPP_KSBOHMM-WO_SER,
      NATION LIKE ZTPP_KSBOHMM-NATION,
      DEALER LIKE ZTPP_KSBOHMM-DEALER,
      EXTC LIKE ZTPP_KSBOHMM-EXTC,
      INTC LIKE ZTPP_KSBOHMM-INTC,
      REMARKS(80),
      END OF IT_COL_ERROR.

RANGES: S_JOBNAM FOR TBTCP-JOBNAME,
        S_PRONAM FOR TBTCP-PROGNAME,
        S_DATE FOR TBTCP-SDLDATE,
        S_TIME FOR TBTCP-SDLTIME.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS: P_COMENT          TYPE C  AS CHECKBOX   DEFAULT 'X',
            P_PROD            TYPE C  AS CHECKBOX   DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK B1.


SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
PARAMETERS: P_ZERO TYPE C AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK B2.

** Changed by Furong on 10/20/08
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME.
PARAMETERS: P_COLC TYPE C AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK B3.
** End of change on 10/20/08

*************** DO NOT USE!!!! *****************************************
DATA: IT_REC                  LIKE TABLE OF MARA ,
      P_TCODE                 LIKE  TSTC-TCODE                ,
      P_CMODE                 TYPE  C                         ,
      P_PMODE                 TYPE  C   VALUE   'N'           ,
      WA_FILENAME             LIKE  RLGRAP-FILENAME,
      WA_FILETYPE             LIKE  RLGRAP-FILETYPE VALUE 'DAT',
      WA_BDCGROUP             LIKE  SY-UNAME.          " APQI-GROUPID
************************************************************************

INITIALIZATION.

LOAD-OF-PROGRAM.

START-OF-SELECTION.
  CHECK P_COMENT = 'X' .
** Changed by Furong on 05/03/07
  IF P_ZERO = 'X'.
    PERFORM SET_ZEROQTY_HMA.
    WAIT UP TO 10 SECONDS.
  ENDIF.
** end of change
  PERFORM GET_DATA.
** Changed by Furong on 10/20/08
  IF P_COLC = 'X'.
    PERFORM COLOR_CONVERSION.
  ENDIF.
  IF NOT IT_COL_ERROR[] IS INITIAL.
    EXIT.
  ENDIF.
** End of change on 10/20/08
  PERFORM WRITE_START      .
  PERFORM PRE_CHECKING_DATA.               " Sequence Quantity Check..
  PERFORM PRE_CHECKING_DATA2.              " OLD Monthly MOD-Qty Check.
  PERFORM BDC_PROCESSING   .
* PERFORM create_finishlog .           " Finish Log ....
  PERFORM WRITE_VIN_SPEC.

END-OF-SELECTION.
  DATA: VARIANT              LIKE INDX-SRTFD VALUE 'ZISD03_01' ,
        EVENTID              LIKE TBTCJOB-EVENTID.

  CHECK P_COMENT = 'X' .
  PERFORM WRITE_SUCCESS.
  PERFORM WRITE_END.
  IF WA_EMPTY IS INITIAL.                                   "UD1K913223
    UPDATE ZTPP_COMMON_VALS SET: DATES = WA_DATE
                               ITEM1 = WA_DATE2
                         WHERE JOBS  = 'ZAPP703C_WORKORDER_MAINT' .
  ENDIF.                                                    "UD1K913223

*  CHECK wa_error = space .
*  WA_DATE = WA_DATE - 1  .
*  EXPORT p_date = wa_date TO DATABASE indx(zz) ID variant.
*  eventid = 'ZISD03_01' .
*
*  CALL FUNCTION 'BP_EVENT_RAISE'
*       EXPORTING
*            eventid                = eventid
*       EXCEPTIONS
*            bad_eventid            = 1
*            eventid_does_not_exist = 2
*            eventid_missing        = 3
*            raise_failed           = 4
*            OTHERS                 = 5.
*
*  IF sy-subrc <> 0.
*    WRITE AT: /001(50) ' Event Call Process was failed.. ',
*              /001(50) '    *** Event ID: ZISD03_01 ***  '.
*  ENDIF.

  PERFORM DELETE_JOBLIST.
** changed by Fuorng on 11/02/05
*  PERFORM delete_data   .
** end of change

  INCLUDE ZCPP103_COMMON_ROUTINE .


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  " Set the Check month Value...
  CLEAR WA_EMPTY.                                           "UD1K913223
  WA_CHECK =  SY-DATUM+2(4).

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
    FROM ZTPP_KSBOHMM .
  WA_TABIX = SY-TABIX.

  IF SY-SUBRC NE 0.
    WA_EMPTY = 'X'.                                         "UD1K913223
    MESSAGE I001 WITH TEXT-015.
    STOP.
  ENDIF.

  SELECT MIN( CHG_DATE ) MAX( CHG_DATE ) INTO (WA_DATE, WA_DATE2)
    FROM ZTPP_KSBOHMM .

*  SELECT SINGLE *  FROM ztpp_common_vals
*   WHERE jobs = 'ZAPP703C_WORKORDER_MAINT' .
*
*  IF sy-subrc = 0 AND ztpp_common_vals-item1(8) < wa_date2 .
*  ELSE.
*    MESSAGE i001 WITH text-015.
*    DELETE FROM ztpp_ksbohmm CLIENT SPECIFIED WHERE mandt = sy-mandt.
*    p_coment = ' '            .
*    STOP.
*  ENDIF.
ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_PROCESSING.
  DATA: L_MODSUM             LIKE ZTPP_KSBOHMM-MODQTY,
        L_CONT               TYPE I        .

  CLEAR:  L_CONT.

  CHECK P_COMENT = 'X' .
  SORT IT_DATA BY WO_SER NATION DEALER EXTC INTC .
  PERFORM GET_LOGSERIAL.
  LOOP AT IT_DATA.
    AT NEW WO_SER.
      C_PROG = 'ZAPP703C_JOBPROGRAM'.
      PERFORM JOB_CREATE.

    ENDAT.
    L_CONT = L_CONT + 1.
  ENDLOOP.
** Added on 09/21/11; furong
  WAIT UP TO 3 SECONDS.
** end on 09/21/11
  PERFORM JOB_CHECK.
ENDFORM.                    " BDC_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  check_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_COLOR  text
*----------------------------------------------------------------------*
FORM CHECK_COLOR USING    PA_COLOR.
  PA_COLOR = 'N'  .
  IF IT_DATA-EXTC = '***' AND IT_DATA-INTC = '***'.
    PA_COLOR = 'Y' .
  ENDIF.
ENDFORM.                    " check_color

*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_EXIST  text
*----------------------------------------------------------------------*
FORM CHECK_MATERIAL USING    PA_EXIST  PA_MATERIAL .
  SELECT SINGLE MATNR INTO PA_MATERIAL
    FROM MARA
   WHERE MATNR = PA_MATERIAL .

  IF SY-SUBRC = 0.
    PA_EXIST = 'Y' .
  ELSE.
    PA_EXIST = 'N' .
  ENDIF.
ENDFORM.                    " CHECK_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0274   text
*      -->P_TXT_001  text
*----------------------------------------------------------------------*
FORM CREATE_LOG USING    PA_TYPE  PA_STEP  PA_TEXT  PA_KEY .
  WA_SEQ = WA_SEQ + 1 .
  IF WA_SEQ = 1       .
    PERFORM GET_LOGSERIAL.               " Log Number Generation........
    ZTPP_PP_LOG_HEAD-LOGKEY   = WA_NUMBER   .
    ZTPP_PP_LOG_HEAD-PROGRAMM = 'ZAPP703C_REPROCESS' .
    ZTPP_PP_LOG_HEAD-LOGTYPE  = PA_TYPE     .
    ZTPP_PP_LOG_HEAD-JOBTYPE  = SY-BATCH    .
    ZTPP_PP_LOG_HEAD-LOGSTEP  = PA_STEP     .
    ZTPP_PP_LOG_HEAD-MSG      = PA_TEXT     .
    ZTPP_PP_LOG_HEAD-LDATE    = SY-DATUM    .
    ZTPP_PP_LOG_HEAD-LTIME    = SY-UZEIT    .
    ZTPP_PP_LOG_HEAD-LUSER    = SY-UNAME    .
    INSERT INTO ZTPP_PP_LOG_HEAD VALUES ZTPP_PP_LOG_HEAD .
  ENDIF.

  " Log Detail Creation
  ZTPP_PP_LOG_DETA-LOGKEY   = WA_NUMBER    .
  ZTPP_PP_LOG_DETA-SEQUENCE = WA_SEQ      .
  ZTPP_PP_LOG_DETA-LOGTYPE  = PA_TYPE     .
  ZTPP_PP_LOG_DETA-KEYDATA  = PA_KEY      .
  INSERT INTO ZTPP_PP_LOG_DETA VALUES ZTPP_PP_LOG_DETA .
ENDFORM.                    " create_log

*&---------------------------------------------------------------------*
*&      Form  GET_LOGSERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LOGSERIAL.
  " Log Head Creation..
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            NR_RANGE_NR             = '01'
            OBJECT                  = 'ZLOG'
       IMPORTING
            NUMBER                  = WA_NUMBER
       EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            BUFFER_OVERFLOW         = 7
            OTHERS                  = 8.
ENDFORM.                    " GET_LOGSERIAL

*&---------------------------------------------------------------------*
*&      Form  create_finishlog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_FINISHLOG.
  " Step 12: Finished creation process.....
  PERFORM CREATE_LOG USING 'E' 12 TEXT-013  SPACE .
ENDFORM.                    " create_finishlog

*&---------------------------------------------------------------------*
*&      Form  create_steplog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_STEPLOG.
  IF WA_ERROR = SPACE.
    " Step 11: Successfully creation...
    PERFORM CREATE_LOG USING 'S' 11 TEXT-011  IT_DATA.
  ELSE.
    " Step 11: Error creation...
    PERFORM CREATE_LOG USING 'E' 11 TEXT-012  IT_DATA.
  ENDIF.
ENDFORM.                    " create_steplog


*&---------------------------------------------------------------------*
*&      Form  check_workorder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_WORKORDER   USING PA_FLAG.
  DATA: L_WORDER                 LIKE TABLE OF IT_DATA WITH HEADER LINE,
        L_CQTY                   LIKE ZTPP_KSBOHMM-INITQTY,  " Color Qty
        L_CONF                   LIKE TABLE OF ZSPP_VIN_VALUE
                                                 WITH HEADER LINE.

  L_WORDER[] = IT_DATA[].
  CLEAR: L_CONF, L_CONF[].
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_MOD_QTY'
       IMPORTING
            OUTPUT = L_CONF-ATINN.
  APPEND L_CONF.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_SEQ_QTY'
       IMPORTING
            OUTPUT = L_CONF-ATINN.
  APPEND L_CONF.

  LOOP AT L_WORDER WHERE WO_SER = IT_DATA-WO_SER  AND
                         NATION = IT_DATA-NATION  AND
                         DEALER = IT_DATA-DEALER  AND
                         EXTC   NE '***'          .
    CONCATENATE IT_DATA-WO_SER IT_DATA-NATION IT_DATA-DEALER
                IT_DATA-EXTC   IT_DATA-INTC   INTO L_WORDER-MATERIAL.


    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = L_WORDER-MATERIAL
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = L_CONF
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    " Compare the Qty....
    READ TABLE L_CONF WITH KEY ATNAM = 'P_SEQ_QTY' .
    L_CQTY = L_CONF-ATWRT.
    IF L_CQTY > L_WORDER-MODQTY.
      " Error.....
      PA_FLAG = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_workorder
*&---------------------------------------------------------------------*
*&      Form  JOB_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM JOB_CREATE.
  REFRESH: GLOBAL_JOB,
           GLOBAL_STEP_TBL.
  CLEAR: GLOBAL_JOB,
         GLOBAL_STEP_TBL.
  WA_COUNT = WA_COUNT + 1.
  CONCATENATE 'WO_CRE_'         WA_COUNT INTO JOBN.

  GLOBAL_JOB-JOBNAME = JOBN.             "S_PROG.
  GLOBAL_JOB-JOBCLASS = 'B'.             "<====== 레벨 C로 지정
  GLOBAL_JOB-NEWFLAG = 'O'.
  GLOBAL_STEP_TBL-PROGRAM = 'RSBTCPT3'.  "dummy step
  GLOBAL_STEP_TBL-TYP = 'A'.             "프로그램실행단계식별
  GLOBAL_STEP_TBL-STATUS = 'P'.          "scheduled
  GLOBAL_STEP_TBL-AUTHCKNAM = SY-UNAME.
  APPEND GLOBAL_STEP_TBL.
  APPEND GLOBAL_JOB.

  CALL FUNCTION 'BP_JOB_CREATE'
     EXPORTING
       JOB_CR_DIALOG             = 'N'
       JOB_CR_HEAD_INP           = GLOBAL_JOB
*       ADK_MODE                  = FALSE
    IMPORTING
      JOB_CR_HEAD_OUT           = GLOBAL_JOB
*       JOB_CR_STDT_OUT           = GLOBAL_START_DATE
     TABLES
       JOB_CR_STEPLIST           = GLOBAL_STEP_TBL
    EXCEPTIONS
        CANT_CREATE_JOB           = 1
        INVALID_DIALOG_TYPE       = 2
        INVALID_JOB_DATA          = 3
        JOB_CREATE_CANCELED       = 4
        OTHERS                    = 5.
  IF SY-SUBRC <> 0.
    WRITE AT: /001(030) 'Error: Job Creation',
          031(010) IT_DATA-WO_SER.
  ENDIF.

  JOBC = GLOBAL_JOB-JOBCOUNT.
  JOBN = GLOBAL_JOB-JOBNAME.

  SUBMIT ZAPP703C_JOBPROGRAM AND RETURN
         WITH P_WOSER EQ IT_DATA-WO_SER
         WITH P_LOG   EQ WA_NUMBER
         VIA JOB JOBN NUMBER JOBC .

  CONCATENATE 'IEQ' JOBN     INTO S_JOBNAM.  APPEND S_JOBNAM.
*  CALL FUNCTION 'JOB_SUBMIT'
*       EXPORTING
*            authcknam = sy-uname
*            jobcount  = jobc
*            jobname   = jobn
*            report    = c_prog
*       EXCEPTIONS
*            OTHERS    = 4.

  CALL FUNCTION 'JOB_CLOSE'
       EXPORTING
            JOBCOUNT  = JOBC
            JOBNAME   = JOBN
            STRTIMMED = IMMEDIATE
       EXCEPTIONS
            OTHERS    = 4.

ENDFORM.                    " JOB_PROCESS
*&---------------------------------------------------------------------*
*&      Form  JOB_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM JOB_CHECK.
  DO.
    CLEAR IT_JOBLIST.
    REFRESH IT_JOBLIST.
    CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
         EXPORTING
              ABAP_PROGRAM_NAME             = C_PROG  "'YIPP_TEST16'
              DIALOG                        = 'N'
         TABLES
              JOBLIST                       = IT_JOBLIST
         EXCEPTIONS
              NO_JOBS_FOUND                 = 1
              PROGRAM_SPECIFICATION_MISSING = 2
              INVALID_DIALOG_TYPE           = 3
              JOB_FIND_CANCELED             = 4
              OTHERS                        = 5.

    IF SY-SUBRC <> 0.     EXIT.     ENDIF.

    READ TABLE IT_JOBLIST WITH KEY JOBNAME(6) = 'WO_CRE'
                                   STATUS = 'S'.
    IF SY-SUBRC = 0.
      CONTINUE.
    ELSE.
      READ TABLE IT_JOBLIST WITH KEY JOBNAME(6) = 'WO_CRE'
                                     STATUS = 'R'.
      IF SY-SUBRC = 0.
        CONTINUE.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDDO.
ENDFORM.                    " JOB_CHECK

*&---------------------------------------------------------------------*
*&      Form  delete_joblist
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_JOBLIST.
  S_PRONAM = 'IEQZAPP703C_JOBPROGRAM'.  APPEND S_PRONAM.

  SUBMIT ZAPP703C_JOBDELETE AND RETURN
         WITH S_JOBNAM   IN S_JOBNAM
         WITH S_PRONAM   IN S_PRONAM .
ENDFORM.                    " delete_joblist

*&---------------------------------------------------------------------*
*&      Form  PRE_CHECKING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_CHECKING_DATA.
  DATA: L_MATNR              LIKE MARA-MATNR,
        L_QTY(5)             TYPE N         ,
        L_LINE               TYPE I         ,
        L_VALS               LIKE TABLE OF ZSPP_VIN_VALUE
                                                       WITH HEADER LINE,
        L_DATA               LIKE TABLE OF IT_DATA     WITH HEADER LINE.

  SORT IT_DATA BY WO_SER NATION DEALER EXTC INTC .
  L_DATA[] = IT_DATA[].
  DELETE L_DATA WHERE EXTC = '***' .

  LOOP AT L_DATA .
    CLEAR: L_VALS, L_VALS[].
    CLEAR: L_DATA-S219,  L_VALS-ATWRT, L_VALS-ATINN, L_VALS-ATBEZ,
           L_VALS-ATWTB, L_VALS-ZFLAG, L_DATA-MOYE .

    CONCATENATE L_DATA-WO_SER L_DATA-NATION L_DATA-DEALER
                L_DATA-EXTC   L_DATA-INTC   INTO L_DATA-MATERIAL.

    L_VALS-ATNAM = 'P_SEQ_QTY'.          APPEND  L_VALS.
    L_VALS-ATNAM = 'P_MOD_QTY'.          APPEND  L_VALS.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = L_DATA-MATERIAL
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = L_VALS
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    IF SY-SUBRC = 0.
      READ TABLE  L_VALS WITH KEY ATNAM = 'P_SEQ_QTY' .     "INDEX  1 .
      L_QTY = L_VALS-ATWRT.
      IF L_QTY > L_DATA-MODQTY .
        L_DATA-S219 = TEXT-003.
        L_DATA-INITQTY = L_QTY.
        L_DATA-MOYE = 'E'     .
      ENDIF.
    ENDIF.
    MODIFY L_DATA.
  ENDLOOP.

  DELETE L_DATA WHERE MOYE = ' '.
  DESCRIBE TABLE L_DATA LINES L_LINE.
  CHECK L_LINE > 0.
*  CLEAR: p_coment .
  WRITE  / TEXT-111.
  SKIP 2 .
  ULINE AT (118) .
  WRITE AT: /001(024) TEXT-102     ,
             025(001) SY-VLINE     ,
             026(015) TEXT-103     ,
             041(001) SY-VLINE     ,
             042(014) TEXT-104     ,
             056(001) SY-VLINE     ,
             057(061) TEXT-105     ,
             118(001) SY-VLINE     .
  ULINE AT: /(118) .

  LOOP AT L_DATA .
    WRITE AT: /001(009) L_DATA-WO_SER,
               010(001) SY-VLINE     ,
               011(003) L_DATA-NATION,
               014(001) SY-VLINE     ,
               015(002) L_DATA-DEALER,
               017(001) SY-VLINE     ,
               018(003) L_DATA-EXTC  ,
               021(001) SY-VLINE     ,
               022(003) L_DATA-INTC  ,
               025(001) SY-VLINE     ,
               026(015) L_DATA-MODQTY,
               041(001) SY-VLINE     ,
               042(014) L_DATA-INITQTY,
               056(001) SY-VLINE     ,
               057(061) L_DATA-S219  ,
               118(001) SY-VLINE     .
  ENDLOOP.
  ULINE AT: /(118) .
*  PERFORM write_end.
ENDFORM.                    " PRE_CHECKING_DATA

*&---------------------------------------------------------------------*
*&      Form  PRE_CHECKING_DATA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_CHECKING_DATA2.
  DATA: L_MATNR              LIKE MARA-MATNR,
        L_QTY(5)             TYPE N         ,
        L_LINE               TYPE I         ,
        L_FLAG               TYPE C         ,
        L_VALS               LIKE TABLE OF ZSPP_VIN_VALUE
                                                       WITH HEADER LINE,
        L_DATA               LIKE TABLE OF IT_DATA     WITH HEADER LINE.

  SORT IT_DATA BY WO_SER NATION DEALER EXTC INTC .
  L_DATA[] = IT_DATA[].
  DELETE L_DATA WHERE EXTC = '***' .

  L_VALS-ATNAM = 'P_MOD_QTY'.          APPEND  L_VALS.

  CHECK P_PROD IS INITIAL   .
  LOOP AT L_DATA .  " where wo_ser < wa_check .   " p_month.
    CHECK L_DATA-WO_SER+1(4) <= WA_CHECK .         " Monthly Pack Check
    CLEAR: L_DATA-S219,  L_VALS-ATWRT, L_VALS-ATINN, L_VALS-ATBEZ,
           L_VALS-ATWTB, L_VALS-ZFLAG, L_DATA-MOYE .

    CONCATENATE L_DATA-WO_SER L_DATA-NATION L_DATA-DEALER
                L_DATA-EXTC   L_DATA-INTC   INTO L_DATA-MATERIAL.

    SELECT SINGLE MATNR INTO L_DATA-MATERIAL
      FROM MARA
     WHERE MATNR = L_DATA-MATERIAL
       AND MTART = 'WOCL'          .

    IF SY-SUBRC NE 0 AND SY-DATUM >= '20040601' .
      L_DATA-S219 = TEXT-016.
      L_DATA-INITQTY = L_QTY.
      L_DATA-MOYE = 'E'     .
      MODIFY L_DATA.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = L_DATA-MATERIAL
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = L_VALS
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    IF SY-SUBRC NE 0.
      L_FLAG = 'X' .
    ELSE.
      CLEAR: L_FLAG.
    ENDIF.
    READ TABLE  L_VALS  INDEX  1 .
    L_QTY = L_VALS-ATWRT.   CLEAR: L_VALS-ATWRT.
    IF L_QTY < L_DATA-MODQTY  OR L_FLAG = 'X'  .
      L_DATA-S219 = TEXT-016.
      L_DATA-INITQTY = L_QTY.
      L_DATA-MOYE = 'E'     .
    ENDIF.
    MODIFY L_DATA.
  ENDLOOP.

  DELETE L_DATA WHERE MOYE NE 'E'.
  DESCRIBE TABLE L_DATA LINES L_LINE.
  CHECK L_LINE > 0.
*  CLEAR: p_coment .
  WRITE  / TEXT-101.
  SKIP 2 .
  ULINE AT (118) .
  WRITE AT: /001(024) TEXT-102     ,
             025(001) SY-VLINE     ,
             026(015) TEXT-103     ,
             041(001) SY-VLINE     ,
             042(014) TEXT-104     ,
             056(001) SY-VLINE     ,
             057(061) TEXT-105     ,
             118(001) SY-VLINE     .
  ULINE AT: /(118) .

  LOOP AT L_DATA .
    WRITE AT: /001(009) L_DATA-WO_SER,
               010(001) SY-VLINE     ,
               011(003) L_DATA-NATION,
               014(001) SY-VLINE     ,
               015(002) L_DATA-DEALER,
               017(001) SY-VLINE     ,
               018(003) L_DATA-EXTC  ,
               021(001) SY-VLINE     ,
               022(003) L_DATA-INTC  ,
               025(001) SY-VLINE     ,
               026(015) L_DATA-MODQTY,
               041(001) SY-VLINE     ,
               042(014) L_DATA-INITQTY,
               056(001) SY-VLINE     ,
               057(061) L_DATA-S219  ,
               118(001) SY-VLINE     .
  ENDLOOP.
  ULINE AT: /(118) .
  PERFORM WRITE_END.
** changed by furong on 11/02/05
*  DELETE FROM ztpp_ksbohmm CLIENT SPECIFIED WHERE mandt = sy-mandt.
** end of change
ENDFORM.                    " PRE_CHECKING_DATA2

*&---------------------------------------------------------------------*
*&      Form  write_start
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_START.
  WRITE :/ TEXT-201 .
  WRITE AT: /001(030) TEXT-202                    ,
             031(010) SY-DATUM                    ,
             042(010) SY-UZEIT                    .
  WRITE :/ TEXT-201 .
  SKIP 1.
ENDFORM.                    " write_start

*&---------------------------------------------------------------------*
*&      Form  WRITE_END
*&---------------------------------------------------------------------*
FORM WRITE_END.
  GET TIME.
  SKIP 2.
  WRITE :/ TEXT-203 .
  WRITE AT: /001(030) TEXT-204                    ,
             031(010) SY-DATUM                    ,
             042(010) SY-UZEIT                    .
  WRITE :/ TEXT-203 .
ENDFORM.                    " WRITE_END

*&---------------------------------------------------------------------*
*&      Form  write_success
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_SUCCESS.
  DATA: L_DATA               LIKE TABLE OF IT_DATA     WITH HEADER LINE,
        L_WOSER              LIKE ZTPP_WOSUM-WO_SER ,
        L_NATION(5)          TYPE C  ,
        L_SUM_HD             TYPE I  ,
        L_SUM_CL             TYPE I  ,
        BEGIN OF L_CHECK     OCCURS 0,
          NATION(5)          TYPE C  ,
          HD_CNT             TYPE I  ,
          CL_CNT             TYPE I  ,
        END OF L_CHECK.

  WRITE : / TEXT-205.
  L_DATA[] = IT_DATA[].
  LOOP AT L_DATA.
    CONCATENATE L_DATA-NATION L_DATA-DEALER INTO L_DATA-MATERIAL .
    MODIFY L_DATA.
  ENDLOOP.

  SORT L_DATA BY MATERIAL  EXTC .
  READ TABLE L_DATA INDEX 1     .
  L_CHECK-NATION = L_NATION = L_DATA-MATERIAL    .

  LOOP AT L_DATA.
    IF L_DATA-MATERIAL = L_NATION.
      IF L_DATA-EXTC  = '***'   .
        L_CHECK-HD_CNT = L_CHECK-HD_CNT + 1.
      ELSE.
        L_CHECK-CL_CNT = L_CHECK-CL_CNT + 1.
      ENDIF.
    ELSE.
      APPEND L_CHECK.
      CLEAR: L_CHECK.
      L_CHECK-NATION = L_NATION = L_DATA-MATERIAL.
      IF L_DATA-EXTC  = '***'   .
        L_CHECK-HD_CNT = 1 .
      ELSE.
        L_CHECK-CL_CNT = 1.
      ENDIF.
    ENDIF.
  ENDLOOP.
  APPEND L_CHECK.

  SKIP 2 .
  WRITE AT: /001(017) TEXT-401     .
  ULINE AT  /(043) .
  WRITE AT: /001(017) TEXT-302     ,
             018(001) SY-VLINE     ,
             019(012) TEXT-303     ,
             031(001) SY-VLINE     ,
             032(011) TEXT-304     ,
             043(001) SY-VLINE     .
  ULINE AT: /(043) .

  LOOP AT L_CHECK.
    L_SUM_HD = L_SUM_HD + L_CHECK-HD_CNT .
    L_SUM_CL = L_SUM_CL + L_CHECK-CL_CNT .
    WRITE AT: /001(017) L_CHECK-NATION,
               018(001) SY-VLINE      ,
               019(012) L_CHECK-HD_CNT,
               031(001) SY-VLINE      ,
               032(011) L_CHECK-CL_CNT,
               043(001) SY-VLINE      .
  ENDLOOP.
  ULINE AT: /(043) .
  WRITE AT: /001(017) TEXT-305      ,
             018(001) SY-VLINE      ,
             019(012) L_SUM_HD      ,
             031(001) SY-VLINE      ,
             032(011) L_SUM_CL      ,
             043(001) SY-VLINE      .
  ULINE AT: /(043) .

  " Work Order Summary data Check...
*  READ TABLE it_data INDEX 1.
*  clear: l_data, l_data[], l_check-hd_cnt, l_check-cl_cnt,
*         l_sum_hd, l_sum_cl, L_CHECK, L_CHECK[] .
*
*  CONCATENATE it_data-wo_ser(5) '%'  INTO l_woser.
*  SELECT * INTO TABLE it_sum
*    FROM ztpp_wosum
*   WHERE wo_ser LIKE l_woser .
*
*  LOOP AT it_sum .
*    move-corresponding it_sum to l_data.
*    CONCATENATE l_data-nation l_data-dealer INTO l_data-material .
*    append l_data.
*  ENDLOOP.

*  SORT l_data BY material  extc .
*  READ TABLE l_data INDEX 1     .
*  l_check-nation = l_nation = l_data-material    .
*
*  LOOP AT l_data.
*    IF l_data-material = l_nation.
*       l_check-cl_cnt = l_check-cl_cnt + 1.
*    ELSE.
*      APPEND l_check.
*      CLEAR: l_check.
*      l_check-nation = l_nation = l_data-material.
*      l_check-cl_cnt = 1.
*    ENDIF.
*  ENDLOOP.
*  APPEND l_check.

*  SKIP 2 .
*  WRITE AT: /001(023) text-402     .
*  ULINE AT  /(043) .
*  WRITE AT: /001(017) text-302     ,
*             018(001) sy-vline     ,
*             019(012) text-303     ,
*             031(001) sy-vline     ,
*             032(011) text-304     ,
*             043(001) sy-vline     .
*  ULINE AT: /(043) .
*
*  LOOP AT l_check.
*    l_sum_hd = l_sum_hd + l_check-hd_cnt .
*    l_sum_cl = l_sum_cl + l_check-cl_cnt .
*    WRITE AT: /001(017) l_check-nation,
*               018(001) sy-vline      ,
*               019(012) l_check-hd_cnt,
*               031(001) sy-vline      ,
*               032(011) l_check-cl_cnt,
*               043(001) sy-vline      .
*  ENDLOOP.
*  ULINE AT: /(043) .
*  WRITE AT: /001(017) text-305      ,
*             018(001) sy-vline      ,
*             019(012) l_sum_hd      ,
*             031(001) sy-vline      ,
*             032(011) l_sum_cl      ,
*             043(001) sy-vline      .
*  ULINE AT: /(043) .
ENDFORM.                    " write_success

*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_DATA.
  DELETE FROM ZTPP_KSBOHMM CLIENT SPECIFIED WHERE MANDT = SY-MANDT .
ENDFORM.                    " DELETE_DATA

*---------------------------------------------------------------------*
*       FORM WRITE_VIN_SPEC                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM WRITE_VIN_SPEC.
  DATA: BEGIN OF LT_DATA OCCURS 0,
        WO_SER LIKE ZTPP_KSBOHMM-WO_SER,
        NATION LIKE ZTPP_KSBOHMM-NATION,
        DEALER LIKE ZTPP_KSBOHMM-DEALER,
        BMDL LIKE ZTPP_KSBOHMM-BMDL,
        OCNN LIKE ZTPP_KSBOHMM-OCNN,
        WORK_ORDER LIKE ZTPP_INPUT_PLAN-WORK_ORDER,
        END OF LT_DATA.

  DATA: L_ATINN LIKE CABN-ATINN,
        LT_AUSP LIKE TABLE OF AUSP WITH HEADER LINE.

  RANGES: S_WORKORDER FOR ZTPP_INPUT_PLAN-WORK_ORDER.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_DATA
   FROM ZTPP_KSBOHMM
    WHERE EXTC = '***'.

  LOOP AT LT_DATA.
    CONCATENATE LT_DATA-WO_SER LT_DATA-NATION LT_DATA-DEALER INTO
             LT_DATA-WORK_ORDER.
    MODIFY LT_DATA.

    S_WORKORDER-LOW = LT_DATA-WORK_ORDER.
    S_WORKORDER-SIGN = 'I'.
    S_WORKORDER-OPTION = 'EQ'.
    APPEND S_WORKORDER.
    CLEAR: S_WORKORDER, LT_DATA.
  ENDLOOP.

  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
    WHERE ATNAM = 'P_VIN_SPEC'.

  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
    WHERE OBJEK IN S_WORKORDER
      AND ATINN = L_ATINN
      AND KLART = '001'.

  LOOP AT LT_DATA.
    READ TABLE LT_AUSP WITH KEY OBJEK = LT_DATA-WORK_ORDER.
    WRITE: / LT_DATA-WORK_ORDER , LT_AUSP-ATWRT,
         LT_DATA-BMDL, LT_DATA-OCNN.
  ENDLOOP.
ENDFORM.                    " WRITE_VIN_SPEC
*&---------------------------------------------------------------------*
*&      Form  set_zeroqty_hma
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ZEROQTY_HMA.
  UPDATE ZTPP_KSBOHMM SET: INITQTY = 0
                           MODQTY = 0.
** On 07/18/13
*                      WHERE DEST = 'B28AA'
*                        OR DEST ='B28AB'.
** End on 07/18/13
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE E001 WITH 'Error occured when setting 0 for ZTPP_KSBOHMM'.
  ENDIF.
ENDFORM.                    " set_zeroqty_hma
*&---------------------------------------------------------------------*
*&      Form  COLOR_CONVERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COLOR_CONVERSION.
  DATA: LT_KSBOHMM LIKE TABLE OF ZTPP_KSBOHMM WITH HEADER LINE.
*  DATA: L_EXTC LIKE ZTPP_KSBOHMM-EXTC,
*        L_INTC LIKE ZTPP_KSBOHMM-INTC,
  DATA:  L_MODL LIKE ZTBM_ABYCOLDT-CTRN_CARS_C,
         L_CONF_INTC LIKE ZTBM_ABYCOLDT-CTRN_CONF_COLR,
         L_CONF_EXTC LIKE ZTBM_ABYCOLDT-CTRN_CONF_COLR,
         L_FLAG(1).

  LOOP AT IT_DATA.
    CLEAR: L_FLAG, L_MODL,L_CONF_INTC, L_CONF_EXTC,
           LT_KSBOHMM, IT_COL_ERROR.
** Externa color check
    IF IT_DATA-EXTC <> '***'.
      L_MODL = IT_DATA-BMDL+0(2).
      SELECT SINGLE CTRN_CONF_COLR INTO L_CONF_EXTC
        FROM ZTBM_ABYCOLDT
        WHERE CTRN_CARS_C = L_MODL
          AND CTRN_YEAR_C2 = IT_DATA-MOYE
          AND CTRN_GUBN_C = 'EXT'
          AND CTRN_KEY_COLR = IT_DATA-EXTC.
      IF SY-SUBRC <> 0.
        MOVE-CORRESPONDING IT_DATA TO IT_COL_ERROR.
        CONCATENATE 'External Color:' IT_DATA-EXTC
                    'not found' INTO IT_COL_ERROR-REMARKS
                    SEPARATED BY SPACE.
        L_FLAG = 'X'.
      ELSE.
        IF L_CONF_EXTC IS INITIAL.
          MOVE-CORRESPONDING IT_DATA TO IT_COL_ERROR.
          CONCATENATE 'No Confirm Color for External Color'
                      IT_DATA-EXTC
                      INTO IT_COL_ERROR-REMARKS
                      SEPARATED BY SPACE.
          L_FLAG = 'X'.
        ELSE.
          LT_KSBOHMM-EXTC = L_CONF_EXTC.
        ENDIF.
      ENDIF.
    ENDIF.
** Internal color check
    IF IT_DATA-INTC <> '***'.
      SELECT SINGLE CTRN_CONF_COLR INTO L_CONF_INTC
        FROM ZTBM_ABYCOLDT
        WHERE CTRN_CARS_C = L_MODL
          AND CTRN_YEAR_C2 = IT_DATA-MOYE
          AND CTRN_GUBN_C = 'INT'
          AND CTRN_KEY_COLR = IT_DATA-INTC.
      IF SY-SUBRC <> 0.
        IF L_FLAG = 'X'.
          CONCATENATE IT_COL_ERROR-REMARKS
                  '/ Internal Color:' IT_DATA-INTC
                  'not found' INTO IT_COL_ERROR-REMARKS
                  SEPARATED BY SPACE.
        ELSE.
          MOVE-CORRESPONDING IT_DATA TO IT_COL_ERROR.
          CONCATENATE 'Internal Color:' IT_DATA-INTC
                      'not found' INTO IT_COL_ERROR-REMARKS
                      SEPARATED BY SPACE.
          L_FLAG = 'X'.
        ENDIF.
      ELSE.
        IF L_CONF_INTC IS INITIAL.
          IF L_FLAG = 'X'.
            CONCATENATE IT_COL_ERROR-REMARKS
                     'No Confirm Color for External Color'
                     IT_DATA-EXTC
                     INTO IT_COL_ERROR-REMARKS
                     SEPARATED BY SPACE.
          ELSE.
            MOVE-CORRESPONDING IT_DATA TO IT_COL_ERROR.
            CONCATENATE 'No Confirm Color for External Color'
                        IT_DATA-EXTC
                        INTO IT_COL_ERROR-REMARKS
                        SEPARATED BY SPACE.
            L_FLAG = 'X'.
          ENDIF.
        ELSE.
          LT_KSBOHMM-EXTC = L_CONF_EXTC.
        ENDIF.
      ENDIF.
    ENDIF.

    IF L_FLAG = 'X'.
      APPEND IT_COL_ERROR.
    ELSE.
      MOVE-CORRESPONDING IT_DATA TO LT_KSBOHMM.
      IF IT_DATA-INTC <> '***'.
        LT_KSBOHMM-INTC = L_CONF_INTC.
      ENDIF.
      IF IT_DATA-EXTC <> '***'.
        LT_KSBOHMM-EXTC = L_CONF_EXTC.
      ENDIF.
      LT_KSBOHMM-ZEDAT = SY-DATUM.
      LT_KSBOHMM-ZETIM = SY-UZEIT.
      APPEND LT_KSBOHMM.
    ENDIF.
  ENDLOOP.
  IF IT_COL_ERROR[] IS INITIAL.
    DELETE FROM ZTPP_KSBOHMM CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
    IF SY-SUBRC = 0.
      COMMIT WORK.
      INSERT ZTPP_KSBOHMM FROM TABLE LT_KSBOHMM.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ELSE.
    WRITE AT: /01(10) 'Work Order',
                12(6)  'Nation',
                20(6)  'Dealer',
                26(3)  'Extc',
                30(3)  'Intc',
                40(80) 'Error'.
    ULINE.

    LOOP AT IT_COL_ERROR.
      WRITE AT: /01(10) IT_COL_ERROR-WO_SER,
                12(6)  IT_COL_ERROR-NATION,
                20(3)  IT_COL_ERROR-DEALER,
                26(3)  IT_COL_ERROR-EXTC,
                30(3)  IT_COL_ERROR-INTC,
                40(80) IT_COL_ERROR-REMARKS.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " COLOR_CONVERSION
