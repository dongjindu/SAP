*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_ZTPPEP2
*   generation date: 06/02/2008 at 13:50:43 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_ZTPPEP2       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
