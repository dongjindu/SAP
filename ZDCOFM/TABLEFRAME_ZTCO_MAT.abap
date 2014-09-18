*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTCO_MAT
*   generation date: 02/28/2013 at 12:32:16 by user HIS20094
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTCO_MAT           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
