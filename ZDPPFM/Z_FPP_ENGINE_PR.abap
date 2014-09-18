FUNCTION Z_FPP_ENGINE_PR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ENGINE STRUCTURE  ZSPPER_RFC
*"----------------------------------------------------------------------
  DATA : IT_ZTPPER    LIKE TABLE OF ZTPPER WITH HEADER LINE .
  DATA : L_TABIX      TYPE SY-TABIX,
         l_EASSYID like t_engine-EASSYID.

*----> Check PP Log
  CLEAR : ZTPP_IF_STATUS .
  SELECT SINGLE *
              FROM ZTPP_IF_STATUS
              WHERE TABNAME EQ 'ZTPPER' .
  IF ZTPP_IF_STATUS-ZGO EQ 'X' .
    T_ENGINE-ZZRET = 'E' .
    MODIFY T_ENGINE TRANSPORTING ZZRET WHERE ZZRET EQ SPACE .
  ENDIF.

  CHECK ZTPP_IF_STATUS-ZGO NE 'X' .
  LOOP AT T_ENGINE .
    MOVE SY-TABIX       TO      L_TABIX           .
    MOVE SY-MANDT       TO      T_ENGINE-MANDT    .
    MOVE 'I'            TO      T_ENGINE-ZRESULT  .
    MOVE-CORRESPONDING T_ENGINE  TO  IT_ZTPPER    .
** Changed by Furong on 06/19/08
    IF T_ENGINE-EASSYID IS INITIAL.
       write: T_ENGINE-rdate to l_EASSYID MM/DD/YYYY.
       concatenate  l_EASSYID+6(4) l_EASSYID+0(2) l_EASSYID+3(2)
                    T_ENGINE-rtime T_ENGINE-RSEQ+1(4)
                    into l_EASSYID.
       IT_ZTPPER-EASSYID = l_EASSYID.
    ENDIF.
** End of change

    INSERT ZTPPER FROM IT_ZTPPER .
    IF SY-SUBRC EQ 0.
      MOVE  'S'         TO      T_ENGINE-ZZRET    .
      MODIFY T_ENGINE   INDEX   L_TABIX           .
    ELSE.
      MOVE  'E'         TO      T_ENGINE-ZZRET    .
      MODIFY T_ENGINE   INDEX   L_TABIX           .
    ENDIF.
    CLEAR : IT_ZTPPER  .
  ENDLOOP.

*  DATA: IT_ENGINE   TYPE ZTPPER   OCCURS 0 WITH HEADER LINE.
*  DATA: IT_ENGINE_H TYPE ZTPPER_H OCCURS 0 WITH HEADER LINE.
*  DATA: EVENTID LIKE TBTCJOB-EVENTID.
*  DATA: L_SLNO       LIKE ZTCA_IF_LOG-ZSLNO,
*        L_JOBNAM     LIKE TBTCJOB-JOBNAME,
*        L_PROGNAM    LIKE SY-REPID,
*        L_INT        LIKE TBTCJOB-JOBCOUNT,
*        L_TABIX      LIKE SY-TABIX,
*        L_TSEQ       LIKE ZTPPER-TSEQ,
*        L_SEQNO(13)  TYPE N,
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
*     OTHERS                        = 8            .
*
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*  LOOP AT T_ENGINE.
*    L_TABIX = SY-TABIX.
**   Check ZTPPER-TSEQ(SEQ No) = '00001' and
**   if condition is error, program out.
*    IF T_ENGINE-TSEQ NE '00001'.
*      SELECT SINGLE MAX( TSEQ )
*             INTO L_TSEQ
*             FROM ZTPPER.
*
**      L_TSEQ = L_TSEQ + 1.
*      IF L_TSEQ >= T_ENGINE-TSEQ.
*        MOVE-CORRESPONDING T_ENGINE TO IT_ENGINE_H.
*        IT_ENGINE_H-EDATE = SY-DATUM.
*        IT_ENGINE_H-ETIME = SY-UZEIT.
*        APPEND IT_ENGINE_H.
*        INSERT ZTPPER_H FROM TABLE IT_ENGINE_H
*                             ACCEPTING DUPLICATE KEYS.
*        IF SY-SUBRC EQ 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*        T_ENGINE-ZZRET = 'E'.
*        T_ENGINE-ZMSG = 'Sequence ERROR'.
*        MODIFY T_ENGINE INDEX L_TABIX.
*        LEAVE PROGRAM.
*      ENDIF.
*    ENDIF.
*
**--> System date + engine SEQ No ==> A
*    CONCATENATE SY-DATUM T_ENGINE-TSEQ INTO L_SEQNO.
*
**   CHECK ZTPPER-FLAG = 'E'
*    SELECT SINGLE * FROM ZTPPER
*           WHERE EASSYID EQ T_ENGINE-EASSYID
*             AND ERPID   EQ T_ENGINE-ERPID
*             AND TSEQ    EQ T_ENGINE-TSEQ
*             AND RSEQ    EQ T_ENGINE
*             AND ZRESULT EQ 'E' .
**             AND FLAG    EQ 'E'.
*
*    IF SY-SUBRC EQ 0.   "If ZTPPER-FLAG = 'E' exist,
**--> Interface date + engine SEQ No ==> B
*      CONCATENATE ZTPPER-ZEDAT ZTPPER-TSEQ INTO L_SEQNO1.
**--> IF System date is equal to Interface date,
*      IF L_SEQNO EQ L_SEQNO1.
*        MOVE-CORRESPONDING T_ENGINE TO IT_ENGINE.
*        APPEND IT_ENGINE.
*        DELETE FROM ZTPPER WHERE EASSYID EQ T_ENGINE-EASSYID
*                             AND ERPID   EQ T_ENGINE-ERPID
*                             AND TSEQ    EQ T_ENGINE-TSEQ
*                             AND RSEQ    EQ T_ENGINE-RSEQ.
*        INSERT INTO ZTPPER VALUES IT_ENGINE.
**        INSERT ZTPPER FROM TABLE IT_ENGINE ACCEPTING DUPLICATE KEYS.
*        IF SY-SUBRC EQ 0.
*          COMMIT WORK.
*          T_ENGINE-ZZRET = 'S'.
*          MODIFY T_ENGINE INDEX L_TABIX.
*        ELSE.
*          ROLLBACK WORK.
*          MOVE-CORRESPONDING T_ENGINE TO IT_ENGINE_H.
*          IT_ENGINE_H-EDATE = SY-DATUM.
*          IT_ENGINE_H-ETIME = SY-UZEIT.
*          APPEND IT_ENGINE_H.
*          INSERT ZTPPER_H FROM TABLE IT_ENGINE_H.
**                               ACCEPTING DUPLICATE KEYS.
*          IF SY-SUBRC EQ 0.
*            COMMIT WORK.
*          ELSE.
*            ROLLBACK WORK.
*          ENDIF.
*          T_ENGINE-ZZRET = 'E'.
*          T_ENGINE-ZMSG = 'Upload ERROR 1'.
*          MODIFY T_ENGINE INDEX L_TABIX.
*          LEAVE PROGRAM.
*        ENDIF.
*
**--> If System date is less than Interface date,
**    previous data with 'E' is Error
*      ELSEIF L_SEQNO < L_SEQNO1.  "
*        MOVE-CORRESPONDING T_ENGINE TO IT_ENGINE_H.
*        IT_ENGINE_H-EDATE = SY-DATUM.
*        IT_ENGINE_H-ETIME = SY-UZEIT.
*        APPEND IT_ENGINE_H.
*        INSERT ZTPPER_H FROM TABLE IT_ENGINE_H
*                             ACCEPTING DUPLICATE KEYS.
*        IF SY-SUBRC EQ 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*        T_ENGINE-ZZRET = 'E'.
*        T_ENGINE-ZMSG = 'Duplicate ERROR'.
*        MODIFY T_ENGINE INDEX L_TABIX.
*        LEAVE PROGRAM.
*
**--> If System dateis greater than Interface date,
*      ELSEIF L_SEQNO > L_SEQNO1.
*        MOVE-CORRESPONDING T_ENGINE TO IT_ENGINE.
*        APPEND IT_ENGINE.
*        INSERT INTO ZTPPER VALUES IT_ENGINE.
**        INSERT ZTPPER FROM TABLE IT_ENGINE ACCEPTING DUPLICATE KEYS.
*        IF SY-SUBRC EQ 0.
*          COMMIT WORK.
*          T_ENGINE-ZZRET = 'S'.
*          MODIFY T_ENGINE INDEX L_TABIX.
*        ELSE.
*          ROLLBACK WORK.
*          MOVE-CORRESPONDING T_ENGINE TO IT_ENGINE_H.
*          IT_ENGINE_H-EDATE = SY-DATUM.
*          IT_ENGINE_H-ETIME = SY-UZEIT.
*          APPEND IT_ENGINE_H.
*          INSERT ZTPPER_H FROM TABLE IT_ENGINE_H
*                               ACCEPTING DUPLICATE KEYS.
*          IF SY-SUBRC EQ 0.
*            COMMIT WORK.
*          ELSE.
*            ROLLBACK WORK.
*          ENDIF.
*          T_ENGINE-ZZRET = 'E'.
*          T_ENGINE-ZMSG = 'Upload ERROR 2'.
*          MODIFY T_ENGINE INDEX L_TABIX.
*          LEAVE PROGRAM.
*        ENDIF.
*      ENDIF.
*
**   If condition is not error.
*    ELSE.
*      SELECT SINGLE * FROM ZTPPER
*             WHERE ZEDAT EQ SY-DATUM
*               AND TSEQ  EQ T_ENGINE-TSEQ
*               AND ZRESULT EQ SPACE .
**               AND FLAG  EQ ' '.
***
*      IF SY-SUBRC EQ 0.
*        CONCATENATE ZTPPER-ZEDAT ZTPPER-TSEQ INTO L_SEQNO1.
*
**  If duplicate data exist, Error and History table create.
*        IF L_SEQNO EQ L_SEQNO1.
*          MOVE-CORRESPONDING T_ENGINE TO IT_ENGINE_H.
*          IT_ENGINE_H-EDATE = SY-DATUM.
*          IT_ENGINE_H-ETIME = SY-UZEIT.
*          APPEND IT_ENGINE_H.
*          INSERT ZTPPER_H FROM TABLE IT_ENGINE_H
*                                     ACCEPTING DUPLICATE KEYS.
*          IF SY-SUBRC EQ 0.
*            COMMIT WORK.
*            T_ENGINE-ZZRET = 'E'.
*            T_ENGINE-ZMSG = 'Duplicate ERROR '.
*            MODIFY T_ENGINE INDEX L_TABIX.
*          ELSE.
*            ROLLBACK WORK.
*          ENDIF.
*
**--> System date is less than interface date,
**    previous data with space is Error
*        ELSEIF L_SEQNO < L_SEQNO1.
*          MOVE-CORRESPONDING T_ENGINE TO IT_ENGINE_H.
*          T_ENGINE-ZZRET = 'E'.
*          T_ENGINE-ZMSG = 'Duplicate ERROR 3'.
*          APPEND IT_ENGINE_H.
*          INSERT ZTPPER_H FROM TABLE IT_ENGINE_H
*                                     ACCEPTING DUPLICATE KEYS.
*          IF SY-SUBRC EQ 0.
*            COMMIT WORK.
*          ELSE.
*            ROLLBACK WORK.
*          ENDIF.
*          MODIFY T_ENGINE INDEX L_TABIX.
*          LEAVE PROGRAM.
*
**--> System date is less than interface date, create
*        ELSEIF L_SEQNO > L_SEQNO1.
*          MOVE-CORRESPONDING T_ENGINE TO IT_ENGINE.
*          APPEND IT_ENGINE.
*          INSERT INTO ZTPPER VALUES IT_ENGINE.
**          INSERT ZTPPER FROM TABLE IT_ENGINE ACCEPTING DUPLICATE KEYS.
*          IF SY-SUBRC EQ 0.
*            COMMIT WORK.
*            T_ENGINE-ZZRET = 'S'.
*            MODIFY T_ENGINE INDEX L_TABIX.
*          ELSE.
*            ROLLBACK WORK.
*            MOVE-CORRESPONDING T_ENGINE TO IT_ENGINE_H.
*            IT_ENGINE_H-EDATE = SY-DATUM.
*            IT_ENGINE_H-ETIME = SY-UZEIT.
*            APPEND IT_ENGINE_H.
*            INSERT ZTPPER_H FROM TABLE IT_ENGINE_H
*                                       ACCEPTING DUPLICATE KEYS.
*            IF SY-SUBRC EQ 0.
*              COMMIT WORK.
*            ELSE.
*              ROLLBACK WORK.
*            ENDIF.
*            T_ENGINE-ZZRET = 'E'.
*            MODIFY T_ENGINE INDEX L_TABIX.
*            LEAVE PROGRAM.
*          ENDIF.
*        ENDIF.
****
*      ELSE.
*        MOVE-CORRESPONDING T_ENGINE TO IT_ENGINE.
*        APPEND IT_ENGINE.
*        INSERT INTO ZTPPER VALUES IT_ENGINE.
**        INSERT ZTPPER FROM TABLE IT_ENGINE. "ACCEPTING DUPLICATE KEYS.
*        IF SY-SUBRC EQ 0.
*          COMMIT WORK.
*          T_ENGINE-ZZRET = 'S'.
*          MODIFY T_ENGINE INDEX L_TABIX.
*        ELSE.
*          ROLLBACK WORK.
*          MOVE-CORRESPONDING T_ENGINE TO IT_ENGINE_H.
*          IT_ENGINE_H-EDATE = SY-DATUM.
*          IT_ENGINE_H-ETIME = SY-UZEIT.
*          APPEND IT_ENGINE_H.
*          INSERT ZTPPER_H FROM TABLE IT_ENGINE_H
*                                     ACCEPTING DUPLICATE KEYS.
*          IF SY-SUBRC EQ 0.
*            COMMIT WORK.
*          ELSE.
*            ROLLBACK WORK.
*          ENDIF.
*          T_ENGINE-ZZRET = 'E'.
*          MODIFY T_ENGINE INDEX L_TABIX.
*          LEAVE PROGRAM.
*        ENDIF.
****
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*** 'ZIPP503L_ENGIN_PR' Program call JOB Execute.
*  L_JOBNAM = 'ENGIN_PR MES -> SAP'.
*  L_PROGNAM = 'ZIPP503I_ENGIN_PR'.
*
*  PERFORM CALL_JOB_OPEN USING L_JOBNAM
*                        CHANGING L_INT.
*
**  EXPORT WA_JOB TO MEMORY ID 'XXYY'.
*  LOOP AT IT_ENGINE.
*    UPDATE ZTPPER SET: ZSLNO    = L_SLNO
*                      JOBCOUNT  = L_INT
*                  WHERE EASSYID = IT_ENGINE-EASSYID
*                    AND ERPID   = IT_ENGINE-ERPID
*                    AND TSEQ    = IT_ENGINE-TSEQ
*                    AND RSEQ    = IT_ENGINE-RSEQ.
*
*    IF SY-SUBRC EQ 0.
*      COMMIT WORK.
*    ELSE.
*      ROLLBACK WORK.
*    ENDIF.
*  ENDLOOP.
*
*  SUBMIT (L_PROGNAM) with S_ZSLNO = l_slno
*                     with R1 = 'X'
*                     USER SY-UNAME
*                     VIA JOB L_JOBNAM
*                     NUMBER L_INT
*                     AND RETURN.
*
**  PERFORM CALL_JOB_SUBMIT USING L_INT
**                                L_JOBNAM
**                                L_PROGNAM.
*
*  PERFORM CALL_JOB_CLOSE USING L_INT
*                               L_JOBNAM.

ENDFUNCTION.
