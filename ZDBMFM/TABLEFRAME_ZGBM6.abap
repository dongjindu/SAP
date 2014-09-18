*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGBM6
*   generation date: 05/15/2004 at 09:38:56 by user WSKIM
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGBM6              .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
