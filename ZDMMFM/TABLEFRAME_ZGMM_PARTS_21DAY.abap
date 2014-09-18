*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_PARTS_21DAY
*   generation date: 03/23/2006 at 16:46:00 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_PARTS_21DAY   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
