*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGQM_VEND_EMAIL
*   generation date: 04/16/2009 at 16:14:08 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGQM_VEND_EMAIL    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
