*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_MOBIS_
*   generation date: 11/06/2009 at 10:38:40 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_MOBIS_        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
