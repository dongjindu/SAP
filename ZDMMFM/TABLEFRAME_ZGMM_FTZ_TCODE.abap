*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_FTZ_TCODE
*   generation date: 08/19/2010 at 15:09:21 by user 100794
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_FTZ_TCODE     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
