*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTPP_PROCESS
*   generation date: 2003.09.25 at 01:19:18 by user SOFTNT
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTPP_PROCESS       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
