*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGBW_MTAB
*   generation date: 05/18/2010 at 16:46:53 by user HIS20018
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGBW_MTAB          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
