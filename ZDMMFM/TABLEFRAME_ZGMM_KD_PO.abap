*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_KD_PO
*   generation date: 08/08/2008 at 09:25:01 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_KD_PO         .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
