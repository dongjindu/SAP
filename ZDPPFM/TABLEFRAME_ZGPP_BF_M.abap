*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_BF_M
*   generation date: 01/07/2004 at 13:19:33 by user WSKIM
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_BF_M          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
