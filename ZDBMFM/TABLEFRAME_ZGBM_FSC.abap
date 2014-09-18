*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGBM_FSC
*   generation date: 02/11/2005 at 13:34:07 by user WSKIM
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGBM_FSC           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
