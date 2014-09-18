*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_SCRAP_CAR
*   generation date: 01/23/2009 at 15:48:17 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_SCRAP_CAR     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
