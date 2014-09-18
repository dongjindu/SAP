*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_CUTOIL
*   generation date: 07-23-2003 at 22:28:47 by user HAKCHIN
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_CUTOIL        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
