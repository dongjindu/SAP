FUNCTION Z_FPP_PRESS_PR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_PRESS STRUCTURE  ZSPPPR_RFC
*"----------------------------------------------------------------------

  DATA : IT_ZTPPPR  LIKE TABLE OF ZTPPPR  WITH HEADER LINE .
  DATA : L_ERROR      TYPE C         ,
         L_TABIX      TYPE SY-TABIX  .

*-----> Check PP Log
  CLEAR : ZTPP_IF_STATUS .
  SELECT SINGLE *
              FROM ZTPP_IF_STATUS
              WHERE TABNAME EQ 'ZTPPPR' .

  IF ZTPP_IF_STATUS-ZGO EQ 'X' .
    T_PRESS-ZZRET = 'E' .
    MODIFY T_PRESS TRANSPORTING ZZRET WHERE ZZRET EQ SPACE .
  ENDIF.

  CHECK ZTPP_IF_STATUS-ZGO NE 'X' .
  LOOP AT T_PRESS .
    MOVE SY-MANDT       TO      T_PRESS-MANDT .
    MOVE 'I'            TO      T_PRESS-ZRESULT  .
    MOVE-CORRESPONDING T_PRESS  TO  IT_ZTPPPR .
    INSERT INTO ZTPPPR VALUES IT_ZTPPPR .
    IF SY-SUBRC EQ 0  AND L_ERROR = SPACE .
      MOVE  'S'         TO      T_PRESS-ZZRET .
    ELSE.
      L_ERROR = 'E' .
      MOVE  'E'         TO      T_PRESS-ZZRET .
    ENDIF.
    MODIFY T_PRESS     .
    CLEAR : IT_ZTPPPR  .
  ENDLOOP.

*  CHECK L_ERROR = SPACE .
*
*  DATA: IT_PRESS TYPE ZTPPPR OCCURS 0 WITH HEADER LINE.
*  DATA: IT_PRESS_H TYPE ZTPPPR_H OCCURS 0 WITH HEADER LINE.
*  DATA: EVENTID LIKE TBTCJOB-EVENTID.
*  DATA: L_SLNO LIKE ZTCA_IF_LOG-ZSLNO,
*        L_JOBNAM LIKE TBTCJOB-JOBNAME,
*        L_PROGNAM LIKE SY-REPID,
*        L_INT LIKE TBTCJOB-JOBCOUNT,
*        L_TSEQ LIKE ZTPPPR-TSEQ,
*        L_SEQNO(13) TYPE N,
*        L_SEQNO1(13) TYPE N.
*
*  CALL FUNCTION 'NUMBER_GET_NEXT'
*    EXPORTING
*      NR_RANGE_NR                   = '01'
*      OBJECT                        = 'ZMESIF'
**   QUANTITY                      = '1'
**   SUBOBJECT                     = ' '
**   TOYEAR                        = '0000'
**   IGNORE_BUFFER                 = ' '
*   IMPORTING
*     NUMBER                        = L_SLNO
**   QUANTITY                      =
**   RETURNCODE                    =
*   EXCEPTIONS
*     INTERVAL_NOT_FOUND            = 1
*     NUMBER_RANGE_NOT_INTERN       = 2
*     OBJECT_NOT_FOUND              = 3
*     QUANTITY_IS_0                 = 4
*     QUANTITY_IS_NOT_1             = 5
*     INTERVAL_OVERFLOW             = 6
*     BUFFER_OVERFLOW               = 7
*     OTHERS                        = 8 .
*
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*  CLEAR: L_TABIX .
*  LOOP AT T_PRESS.
*    L_TABIX = L_TABIX + 1 .
*    IF T_PRESS-TSEQ NE '00001'.
*      SELECT SINGLE MAX( TSEQ )  INTO L_TSEQ
*        FROM ZTPPPR.
*
*      IF L_TSEQ >= T_PRESS-TSEQ.
*        MOVE-CORRESPONDING T_PRESS TO IT_PRESS_H.
*        IT_PRESS_H-EDATE = SY-DATUM.
*        IT_PRESS_H-ETIME = SY-UZEIT.
*        APPEND IT_PRESS_H.
*        INSERT ZTPPPR_H FROM TABLE IT_PRESS_H
*                             ACCEPTING DUPLICATE KEYS.
*        IF SY-SUBRC NE 0.
*          L_ERROR = 'E' .
*          ROLLBACK WORK.
*          EXIT.
*        ENDIF.
*        T_PRESS-ZZRET = 'E'.
*        T_PRESS-ZMSG = 'Sequence ERROR'.
*        MODIFY T_PRESS INDEX L_TABIX.
*        LEAVE PROGRAM.
*      ENDIF.
*    ENDIF.
*
**--> System date + PRESS SEQ No
*    CONCATENATE SY-DATUM T_PRESS-TSEQ INTO L_SEQNO.
**   CHECK ZTPPPR-FLAG = 'E'
*    SELECT SINGLE * FROM ZTPPPR
*           WHERE PNLNO EQ T_PRESS-PNLNO
*             AND PRPID   EQ T_PRESS-PRPID
*             AND TSEQ    EQ T_PRESS-TSEQ
*             AND RSEQ    EQ T_PRESS
*             AND ZRESULT EQ 'E'.
*
*    IF SY-SUBRC EQ 0.
**--> Interface date + engine SEQ No
*      CONCATENATE ZTPPPR-ZEDAT ZTPPPR-TSEQ INTO L_SEQNO1.
**--> IF System date is equal to Interface date,
*      IF L_SEQNO EQ L_SEQNO1.
*        MOVE-CORRESPONDING T_PRESS TO IT_PRESS.
*        APPEND IT_PRESS.
*        DELETE FROM ZTPPPR WHERE PNLNO EQ T_PRESS-PNLNO
*                             AND PRPID   EQ T_PRESS-PRPID
*                             AND TSEQ    EQ T_PRESS-TSEQ
*                             AND RSEQ    EQ T_PRESS-RSEQ.
*        INSERT ZTPPPR FROM TABLE IT_PRESS ACCEPTING DUPLICATE KEYS.
*        IF SY-SUBRC EQ 0.
*          T_PRESS-ZZRET = 'S'.
*          MODIFY T_PRESS INDEX L_TABIX.
*        ELSE.
*          ROLLBACK WORK.
*          MOVE-CORRESPONDING T_PRESS TO IT_PRESS_H.
*          IT_PRESS_H-EDATE = SY-DATUM.
*          IT_PRESS_H-ETIME = SY-UZEIT.
*          APPEND IT_PRESS_H.
*          INSERT ZTPPPR_H FROM TABLE IT_PRESS_H
*                               ACCEPTING DUPLICATE KEYS.
*          IF SY-SUBRC EQ 0.
*            COMMIT WORK.
*          ELSE.
*            ROLLBACK WORK.
*          ENDIF.
*          T_PRESS-ZZRET = 'E'.
*          T_PRESS-ZMSG = 'Upload ERROR 1'.
*          MODIFY T_PRESS INDEX L_TABIX.
*          LEAVE PROGRAM.
*        ENDIF.
*
**--> If System date is less than Interface date,
**    previous data with 'E' is Error
*      ELSEIF L_SEQNO < L_SEQNO1.
*        MOVE-CORRESPONDING T_PRESS TO IT_PRESS_H.
*        IT_PRESS_H-EDATE = SY-DATUM.
*        IT_PRESS_H-ETIME = SY-UZEIT.
*        APPEND IT_PRESS_H.
*        INSERT ZTPPPR_H FROM TABLE IT_PRESS_H
*                             ACCEPTING DUPLICATE KEYS.
*        IF SY-SUBRC EQ 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*        T_PRESS-ZZRET = 'E'.
*        T_PRESS-ZMSG = 'Duplicate ERROR'.
*        MODIFY T_PRESS INDEX L_TABIX.
*        LEAVE PROGRAM.
*
**--> If System dateis greater than Interface date,
*      ELSEIF L_SEQNO > L_SEQNO1.
*        MOVE-CORRESPONDING T_PRESS TO IT_PRESS.
*        APPEND IT_PRESS.
*        INSERT ZTPPPR FROM TABLE IT_PRESS ACCEPTING DUPLICATE KEYS.
*        IF SY-SUBRC EQ 0.
*          COMMIT WORK.
*          T_PRESS-ZZRET = 'S'.
*          MODIFY T_PRESS INDEX L_TABIX.
*        ELSE.
*          ROLLBACK WORK.
*          MOVE-CORRESPONDING T_PRESS TO IT_PRESS_H.
*          IT_PRESS_H-EDATE = SY-DATUM.
*          IT_PRESS_H-ETIME = SY-UZEIT.
*          APPEND IT_PRESS_H.
*          INSERT ZTPPPR_H FROM TABLE IT_PRESS_H
*                               ACCEPTING DUPLICATE KEYS.
*          IF SY-SUBRC EQ 0.
*            COMMIT WORK.
*          ELSE.
*            ROLLBACK WORK.
*          ENDIF.
*          T_PRESS-ZZRET = 'E'.
*          T_PRESS-ZMSG = 'Upload ERROR 2'.
*          MODIFY T_PRESS INDEX L_TABIX.
*          LEAVE PROGRAM.
*        ENDIF.
*      ENDIF.
*
**   If condition is not error.
*    ELSE.
*      SELECT SINGLE * FROM ZTPPPR
*             WHERE ZEDAT    EQ SY-DATUM
*               AND TSEQ     EQ T_PRESS-TSEQ
*               AND ZRESULT  EQ SPACE.
*      IF SY-SUBRC EQ 0.
*        CONCATENATE ZTPPPR-ZEDAT ZTPPPR-TSEQ INTO L_SEQNO1.
*
**  If duplicate data exist, Error and History table create.
*        IF L_SEQNO EQ L_SEQNO1.
*          MOVE-CORRESPONDING T_PRESS TO IT_PRESS_H.
*          IT_PRESS_H-EDATE = SY-DATUM.
*          IT_PRESS_H-ETIME = SY-UZEIT.
*          APPEND IT_PRESS_H.
*          INSERT ZTPPPR_H FROM TABLE IT_PRESS_H
*                                     ACCEPTING DUPLICATE KEYS.
*          IF SY-SUBRC EQ 0.
*            COMMIT WORK.
*            T_PRESS-ZZRET = 'E'.
*            T_PRESS-ZMSG = 'Duplicate ERROR '.
*            MODIFY T_PRESS INDEX L_TABIX.
*          ELSE.
*            ROLLBACK WORK.
*          ENDIF.
*
**--> System date is less than interface date,
**    previous data with space is Error
*        ELSEIF L_SEQNO < L_SEQNO1.
*          MOVE-CORRESPONDING T_PRESS TO IT_PRESS_H.
*          T_PRESS-ZZRET = 'E'.
*          T_PRESS-ZMSG = 'Duplicate ERROR 3'.
*          APPEND IT_PRESS_H.
*          INSERT ZTPPPR_H FROM TABLE IT_PRESS_H
*                                     ACCEPTING DUPLICATE KEYS.
*          IF SY-SUBRC EQ 0.
*            COMMIT WORK.
*          ELSE.
*            ROLLBACK WORK.
*          ENDIF.
*          MODIFY T_PRESS INDEX L_TABIX.
*          LEAVE PROGRAM.
*
**--> System date is less than interface date, create
*        ELSEIF L_SEQNO > L_SEQNO1.
*          MOVE-CORRESPONDING T_PRESS TO IT_PRESS.
*          APPEND IT_PRESS.
*          INSERT ZTPPPR FROM TABLE IT_PRESS ACCEPTING DUPLICATE KEYS.
*          IF SY-SUBRC EQ 0.
*            COMMIT WORK.
*            T_PRESS-ZZRET = 'S'.
*            MODIFY T_PRESS INDEX L_TABIX.
*          ELSE.
*            ROLLBACK WORK.
*            MOVE-CORRESPONDING T_PRESS TO IT_PRESS_H.
*            IT_PRESS_H-EDATE = SY-DATUM.
*            IT_PRESS_H-ETIME = SY-UZEIT.
*            APPEND IT_PRESS_H.
*            INSERT ZTPPPR_H FROM TABLE IT_PRESS_H
*                                       ACCEPTING DUPLICATE KEYS.
*            IF SY-SUBRC EQ 0.
*              COMMIT WORK.
*            ELSE.
*              ROLLBACK WORK.
*            ENDIF.
*            T_PRESS-ZZRET = 'E'.
*            MODIFY T_PRESS INDEX L_TABIX.
*            LEAVE PROGRAM.
*          ENDIF.
*        ENDIF.
****
*      ELSE.
*        MOVE-CORRESPONDING T_PRESS TO IT_PRESS.
*        APPEND IT_PRESS.
*        INSERT INTO ZTPPPR VALUES IT_PRESS.
**        INSERT ZTPPER FROM TABLE IT_PRESS. "ACCEPTING DUPLICATE KEYS.
*        IF SY-SUBRC EQ 0.
*          COMMIT WORK.
*          T_PRESS-ZZRET = 'S'.
*          MODIFY T_PRESS INDEX L_TABIX.
*        ELSE.
*          ROLLBACK WORK.
*          MOVE-CORRESPONDING T_PRESS TO IT_PRESS_H.
*          IT_PRESS_H-EDATE = SY-DATUM.
*          IT_PRESS_H-ETIME = SY-UZEIT.
*          APPEND IT_PRESS_H.
*          INSERT ZTPPPR_H FROM TABLE IT_PRESS_H
*                                     ACCEPTING DUPLICATE KEYS.
*          IF SY-SUBRC EQ 0.
*            COMMIT WORK.
*          ELSE.
*            ROLLBACK WORK.
*          ENDIF.
*          T_PRESS-ZZRET = 'E'.
*          MODIFY T_PRESS INDEX L_TABIX.
*          LEAVE PROGRAM.
*        ENDIF.
****
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

*  LOOP AT T_PRESS .
*    L_TABIX = SY-TABIX .
*    MOVE-CORRESPONDING T_PRESS TO IT_PRESS   .
*    APPEND IT_PRESS .       CLEAR IT_PRESS   .
*    MOVE SY-MANDT     TO  T_PRESS-MANDT      .
*    MODIFY T_PRESS    INDEX   L_TABIX        .
*
*    CLEAR ZTPPPR .
*    SELECT SINGLE *
*                 FROM ZTPPPR
**                 WHERE TSEQ    EQ T_PRESS-TSEQ       "TSEQ
*                  WHERE PRPID   EQ T_PRESS-PRPID      "RPONIT
**                   AND RSEQ    EQ T_PRESS-RSEQ       "SEQ
*                    AND PNLNO   EQ T_PRESS-PNLNO      "MATNR #
*                    AND PPRDNO  EQ T_PRESS-PPRDNO .   "PRD ORD
*    IF SY-SUBRC EQ 0.
*      DELETE FROM ZTPPPR
**                 WHERE TSEQ    EQ T_PRESS-TSEQ       "TSEQ
*                  WHERE PRPID   EQ T_PRESS-PRPID      "RPONIT
**                   AND RSEQ    EQ T_PRESS-RSEQ       "SEQ
*                    AND PNLNO   EQ T_PRESS-PNLNO      "MATNR #
*                    AND PPRDNO  EQ T_PRESS-PPRDNO .   "PRD ORD
*      MOVE : ZTPPPR-MBLNR  TO  IT_PRESS-MBLNR   ,
*             ZTPPPR-MJAHR  TO  IT_PRESS-MJAHR   .
*    ELSE.
*
*    ENDIF.
*
*
*    MODIFY ZTPPPR FROM IT_PRESS .
*    IF SY-SUBRC EQ 0.
*      T_PRESS-ZZRET = 'S' .
*    ELSE.
*      T_PRESS-ZZRET = 'E' .
*    ENDIF.
*    MODIFY T_PRESS TRANSPORTING ZZRET WHERE MANDT EQ SY-MANDT .
*  ENDLOOP .

*** 'ZIPP603L_PRESS_PR' Program call JOB Execute.
*  L_JOBNAM = 'PRESS_PR MES -> SAP'.
*  L_PROGNAM = 'ZIPP603I_PRESS_PR'.
*
*  PERFORM CALL_JOB_OPEN USING L_JOBNAM
*                        CHANGING L_INT.
*
**  EXPORT WA_JOB TO MEMORY ID 'XXYY'.
*  LOOP AT IT_PRESS.
*    UPDATE ZTPPPR SET: ZSLNO    = L_SLNO
*                       JOBCOUNT = L_INT
*                  WHERE PNLNO   = IT_PRESS-PNLNO
*                    AND PPRDNO  = IT_PRESS-PPRDNO
*                    AND PRPID   = IT_PRESS-PRPID.
*    IF SY-SUBRC EQ 0.
*      COMMIT WORK.
*    ELSE.
*      ROLLBACK WORK.
*    ENDIF.
*  ENDLOOP.
*
*  SUBMIT (L_PROGNAM) WITH S_ZSLNO = L_SLNO
*                     WITH R1 = 'X'
*                     USER SY-UNAME
*                     VIA JOB L_JOBNAM
*                     NUMBER L_INT
*                     AND RETURN.
*
*  PERFORM CALL_JOB_CLOSE USING L_INT
*                               L_JOBNAM.
ENDFUNCTION.
