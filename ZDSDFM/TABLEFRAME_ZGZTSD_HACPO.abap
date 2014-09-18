*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGZTSD_HACPO
*   generation date: 06/07/2007 at 10:52:55 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGZTSD_HACPO       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
