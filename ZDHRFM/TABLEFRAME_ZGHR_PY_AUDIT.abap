*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGHR_PY_AUDIT
*   generation date: 07/17/2014 at 10:38:41 by user HIS20037
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGHR_PY_AUDIT      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
