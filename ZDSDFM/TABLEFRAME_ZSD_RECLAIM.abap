*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSD_RECLAIM
*   generation date: 11/01/2006 at 10:56:52 by user P00181
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSD_RECLAIM        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
