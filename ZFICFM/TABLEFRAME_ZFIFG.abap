*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFIFG
*   generation date: 2003.07.17 at 23:51:56 by user ANDY
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFIFG              .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
