*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_TVCD
*   generation date: 07-20-2003 at 23:27:25 by user HAKCHIN
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_TVCD          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
