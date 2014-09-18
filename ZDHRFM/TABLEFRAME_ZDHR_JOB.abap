*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZDHR_JOB
*   generation date: 09/29/2006 at 17:13:18 by user 103088
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZDHR_JOB           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
