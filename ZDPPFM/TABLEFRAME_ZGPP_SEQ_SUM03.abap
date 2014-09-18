*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_SEQ_SUM03
*   generation date: 06/22/2009 at 11:28:34 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_SEQ_SUM03     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
