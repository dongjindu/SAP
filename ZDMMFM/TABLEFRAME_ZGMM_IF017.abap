*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_IF017
*   generation date: 06/04/2009 at 17:17:11 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_IF017         .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
