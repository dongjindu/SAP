*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_WOSUM
*   generation date: 11/19/2003 at 03:42:21 by user DAY7
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_WOSUM         .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
