*&------------------------------------------------------------------
*& Program ID     : ZMMR90500T_CANCEL_RESV
*& Profram Name   : Batch Job Program for sending cancelled reservation
*& Created by     :
*& Created on     : 10/06/11
*& Development ID :
*& Reference Pgm. :
*& Description    : copy from ZMMR90500T
*&
*& Modification Log
*&====================================================================
*& Date        Developer      Request ID      Description
*&
*&--------------------------------------------------------------------
REPORT ZMMR90500T_CANCEL_RESV LINE-SIZE 154 LINE-COUNT 58
                      MESSAGE-ID ZMMM
                      NO STANDARD PAGE HEADING.

TABLES : ZMMT0036, RALDB.

DATA : E_RETURN LIKE ZMMS0053,
       IT_M038  LIKE ZMMT0038 OCCURS 0 WITH HEADER LINE.

DATA: GV_CNT TYPE I.

SELECTION-SCREEN BEGIN OF BLOCK BK1 WITH FRAME TITLE TEXT-SEL.

PARAMETERS:     P_SHOP  LIKE ZMMT0051-PTYPE DEFAULT 'T'      MODIF ID
NOI,
                P_MTART LIKE MARA-MTART     DEFAULT 'ROH'   MODIF ID
NOI,
                P_DATE  LIKE SY-DATUM,"   DEFAULT SY-DATUM MODIF ID noI,
                P_TDATE LIKE SY-DATUM,
                P_FLAG  TYPE UPDKZ.

SELECTION-SCREEN END OF BLOCK BK1.

PARAMETERS: P_VAR TYPE RALDB-VARIANT DEFAULT 'CANCEL_ORDERS'.


AT SELECTION-SCREEN OUTPUT.
*  PERFORM MODIFY_SELECTION_SCREEN.

*--------------------------------------------------------------------*
*   INITIALIZATION                                                   *
*--------------------------------------------------------------------*
INITIALIZATION.
*  PERFORM INITIALIZATION.

***********************************************************************
* AT SELECTION-SCREEN Event                                            *
************************************************************************
*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM CHECKING_BATCH_JOB.

  CLEAR GV_CNT.
  PERFORM SEND_FEEDING_ORDER_TO_GCS.

*&---------------------------------------------------------------------*
*&      Form  send_feeding_order_to_gcs
*&---------------------------------------------------------------------*
FORM SEND_FEEDING_ORDER_TO_GCS .

*  WAIT UP TO 120 SECONDS.
  CLEAR : IT_M038, IT_M038[].

  SELECT  *
        INTO CORRESPONDING FIELDS OF TABLE IT_M038
        FROM ZMMT0038
        WHERE ( ( TYPE  IN (' ', 'E') AND ZFEEDER <> 'HOT' )
           OR ( TYPE  EQ 'E' AND ZFEEDER EQ 'HOT' ) )
           AND REVERSED = 'X'
** Furong on 05/22/12 for tuning
         %_HINTS ORACLE 'INDEX (ZMMT0038 "ZMMT0038~Z03")'.
** End on 05/22/12

*  SORT IT_M038 BY REVERSED.

  IT_M038-TYPE     = 'R'.
  IT_M038-MESSAGE  = 'Batch job Release'.
  MODIFY IT_M038 TRANSPORTING TYPE MESSAGE
                        WHERE TYPE = ' '
                           OR TYPE = 'E'.

  MODIFY ZMMT0038 FROM TABLE IT_M038.
  COMMIT WORK.

  IF NOT IT_M038[] IS INITIAL.

    CALL FUNCTION 'Z_MM_IF_OB_02_003_RE'
         IMPORTING
              E_RETURN = E_RETURN
         TABLES
              IT_BODY  = IT_M038.

    DESCRIBE TABLE IT_M038 LINES GV_CNT.
    MESSAGE S999 WITH GV_CNT 'Data(s) was sent..'.
  ELSE.
    MESSAGE I999 WITH 'No Data found.'.
  ENDIF.

*  PERFORM LOCK_OBJECT USING 'D' 'R11'.
ENDFORM.                    " send_feeding_order_to_gcs

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZATION .
*  IF SY-UNAME(4) = 'GLVS'.
*    CLEAR: P_R01, P_R02, P_R03, P_R04, P_R06, P_R07, P_R08,
*           P_R09, P_R10, P_R11, P_R12, P_R13, P_R14.
*    MOVE: 'X' TO P_R05.
*  ENDIF.
ENDFORM.                    " INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  LOCK_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0355   text
*      -->P_0356   text
*----------------------------------------------------------------------*
FORM LOCK_OBJECT USING    P_CHK
                          P_R.

*  IF P_CHK = 'E'.
*    CALL FUNCTION 'ENQUEUE_EZ_ZMMT0038'
*         EXPORTING
*              RADIO          = P_R
*         EXCEPTIONS
*              FOREIGN_LOCK   = 1
*              SYSTEM_FAILURE = 2
*              OTHERS         = 3.
*
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      LEAVE PROGRAM.
*    ENDIF.
*
*  ELSE.
*    CALL FUNCTION 'DEQUEUE_EZ_ZMMT0038'
*         EXPORTING
*              RADIO = P_R.
*  ENDIF.
ENDFORM.                    " LOCK_OBJECT


*---------------------------------------------------------------------*
*       FORM CHECKING_BATCH_JOB                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CHECKING_BATCH_JOB.
  DATA: L_BACKJOB LIKE  SY-REPID,
        L_VARIANT LIKE RALDB-VARIANT,
        LT_JOBLIST LIKE TBTCJOB OCCURS 0 WITH HEADER LINE.

  L_BACKJOB = SY-REPID.
**?????????

  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
       EXPORTING
            ABAP_PROGRAM_NAME             = L_BACKJOB
            ABAP_VARIANT_NAME             = P_VAR
*            DIALOG                        = 'N'
            STATUS                        = 'R'
       TABLES
            JOBLIST                       = LT_JOBLIST
       EXCEPTIONS
            NO_JOBS_FOUND                 = 1
            PROGRAM_SPECIFICATION_MISSING = 2
            INVALID_DIALOG_TYPE           = 3
            JOB_FIND_CANCELED             = 4
            OTHERS                        = 5.

 IF SY-BATCH EQ 'X'.
    READ TABLE LT_JOBLIST INDEX 2.
    IF SY-SUBRC EQ 0.
      MESSAGE S999 WITH TEXT-M01.
      LEAVE PROGRAM.
    ENDIF.
  ELSE.
    READ TABLE LT_JOBLIST INDEX 1.
     IF SY-SUBRC EQ 0.
      MESSAGE S999 WITH TEXT-M01.
      LEAVE PROGRAM.
    ENDIF.

*    IF SY-SUBRC EQ 0.
*      MESSAGE E999 WITH TEXT-M01.
*    ENDIF.
  ENDIF.

  L_BACKJOB = 'ZMMR90500T'.
**?????????
  L_VARIANT = 'FEED_ORDER_GCS'.

  REFRESH: LT_JOBLIST.

  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
       EXPORTING
            ABAP_PROGRAM_NAME             = L_BACKJOB
            ABAP_VARIANT_NAME             = L_VARIANT
*            DIALOG                        = 'N'
            STATUS                        = 'R'
       TABLES
            JOBLIST                       = LT_JOBLIST
       EXCEPTIONS
            NO_JOBS_FOUND                 = 1
            PROGRAM_SPECIFICATION_MISSING = 2
            INVALID_DIALOG_TYPE           = 3
            JOB_FIND_CANCELED             = 4
            OTHERS                        = 5.

 IF SY-BATCH EQ 'X'.
    READ TABLE LT_JOBLIST INDEX 2.
    IF SY-SUBRC EQ 0.
      MESSAGE S999 WITH
       'This job is terminated because job '
       L_BACKJOB ' is running'.
      LEAVE PROGRAM.
    ENDIF.
  ELSE.
    READ TABLE LT_JOBLIST INDEX 1.
    IF SY-SUBRC EQ 0.
      MESSAGE S999 WITH
       'This job is terminated because job '
       L_BACKJOB ' is running'.
      LEAVE PROGRAM.
    ENDIF.
*    IF SY-SUBRC EQ 0.
*      MESSAGE E999 WITH
*      'This job is terminated because job '
*       L_BACKJOB ' is running'.
*    ENDIF.
  ENDIF.

ENDFORM.                    " checking_batch_job
