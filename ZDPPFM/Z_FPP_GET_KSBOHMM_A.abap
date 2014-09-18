FUNCTION Z_FPP_GET_KSBOHMM_A.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(DBCNT) LIKE  SY-DBCNT
*"  EXPORTING
*"     VALUE(ZZRET) TYPE  ZZRET
*"----------------------------------------------------------------------

  DATA: WA_JOBCOUNT LIKE  TBTCJOB-JOBCOUNT ,
        WA_JOBNAME  LIKE  TBTCJOB-JOBNAME
                    VALUE 'ZAPP703C_WORKORDER_MAINT_SHARE',
        WA_REPORT   LIKE  SY-REPID,
        WA_DBCNT_IX LIKE  SY-TABIX.

  SELECT COUNT( * )
         INTO WA_DBCNT_IX
         FROM ZTPP_KSBOHMM
         CLIENT SPECIFIED
         WHERE MANDT EQ SY-MANDT.

  IF DBCNT NE WA_DBCNT_IX.
    ZZRET = 'E'.
*    DELETE FROM ZTPP_KSBOHMM
*           CLIENT SPECIFIED
*           WHERE MANDT EQ SY-MANDT.
*
  ELSE.
    ZZRET = 'S'.      "SUCCESS
    MOVE WA_JOBNAME    TO    WA_REPORT.

*-----> JOB OPEN
*    PERFORM CALL_JOB_OPEN USING WA_JOBNAME WA_JOBCOUNT.
*
**-----> JOB SUBMIT
*    SUBMIT (WA_REPORT) WITH P_JOBC EQ WA_JOBCOUNT
*                       WITH R1 EQ 'X'
*                       WITH R2 EQ SPACE
*                     USER SY-UNAME
*                     VIA JOB WA_JOBNAME
*                     NUMBER WA_JOBCOUNT
*                     AND RETURN.

*    PERFORM CALL_JOB_SUBMIT USING WA_JOBNAME
*                                  WA_REPORT
*                                  WA_JOBCOUNT.

*-----> JOB CLOSE
*    PERFORM CALL_JOB_CLOSE USING WA_JOBNAME
*                                 WA_JOBCOUNT.

*-----> Execute Sequence File Separation
*    SUBMIT ZAPP703C_WORKORDER_MAINT_SHARE
*                        WITH p_coment = 'X'.
*    IF SY-SUBRC NE 0.
*
*    ENDIF.
  ENDIF.
ENDFUNCTION.
