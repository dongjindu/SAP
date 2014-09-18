*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGRF_MM
*   generation date: 08/29/2005 at 08:10:08 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGRF_MM            .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
