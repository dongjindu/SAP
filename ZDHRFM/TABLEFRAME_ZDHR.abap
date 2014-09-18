*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZDHR
*   generation date: 03/24/2008 at 13:36:08 by user 103569
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZDHR               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
