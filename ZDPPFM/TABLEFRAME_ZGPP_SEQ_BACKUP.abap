*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_SEQ_BACKUP
*   generation date: 05/28/2009 at 10:10:33 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_SEQ_BACKUP    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
