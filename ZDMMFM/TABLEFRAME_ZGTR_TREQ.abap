*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGTR_TREQ
*   generation date: 09/24/2009 at 16:09:54 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGTR_TREQ          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
