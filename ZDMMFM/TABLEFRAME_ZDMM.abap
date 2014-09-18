*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZDMM
*   generation date: 11/13/2003 at 02:45:26 by user HJSONG
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZDMM               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
