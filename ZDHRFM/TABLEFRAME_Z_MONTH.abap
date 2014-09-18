*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_Z_MONTH
*   generation date: 09/21/2004 at 01:32:29 by user 9263014
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_Z_MONTH            .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
