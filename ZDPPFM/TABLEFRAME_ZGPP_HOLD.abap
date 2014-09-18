*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_HOLD
*   generation date: 03/30/2005 at 10:20:32 by user WSKIM
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_HOLD          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
