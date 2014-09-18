*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_CHANGE
*   generation date: 12/15/2003 at 19:01:09 by user SOFTNT
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_CHANGE        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
