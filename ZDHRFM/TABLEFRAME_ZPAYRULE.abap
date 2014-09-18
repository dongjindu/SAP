*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZPAYRULE
*   generation date: 04/13/2012 at 14:24:40 by user HIS20094
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZPAYRULE           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
