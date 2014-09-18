*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_Z_MM_PDA
*   generation date: 06/14/2011 at 17:33:06 by user T00189
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_Z_MM_PDA           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
